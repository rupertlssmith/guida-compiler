module Compiler.Type.Unify exposing
    ( Answer(..)
    , unify
    )

import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Type.Error as Error
import Compiler.Type.Occurs as Occurs
import Compiler.Type.Type as Type
import Compiler.Type.UnionFind as UF
import Data.IO as IO exposing (IO)
import Data.Map as Dict exposing (Dict)
import Utils.Main as Utils



-- UNIFY


type Answer
    = AnswerOk (List UF.Variable)
    | AnswerErr (List UF.Variable) Error.Type Error.Type


unify : UF.Variable -> UF.Variable -> IO Answer
unify v1 v2 =
    case guardedUnify v1 v2 of
        Unify k ->
            k []
                |> IO.bind
                    (\result ->
                        case result of
                            Ok (UnifyOk vars ()) ->
                                onSuccess vars ()

                            Err (UnifyErr vars ()) ->
                                Type.toErrorType v1
                                    |> IO.bind
                                        (\t1 ->
                                            Type.toErrorType v2
                                                |> IO.bind
                                                    (\t2 ->
                                                        UF.union v1 v2 errorDescriptor
                                                            |> IO.fmap (\_ -> AnswerErr vars t1 t2)
                                                    )
                                        )
                    )


onSuccess : List UF.Variable -> () -> IO Answer
onSuccess vars () =
    IO.pure (AnswerOk vars)


errorDescriptor : UF.Descriptor
errorDescriptor =
    UF.Descriptor UF.Error Type.noRank Type.noMark Nothing



-- CPS UNIFIER


type Unify a
    = Unify (List UF.Variable -> IO (Result UnifyErr (UnifyOk a)))


type UnifyOk a
    = UnifyOk (List UF.Variable) a


type UnifyErr
    = UnifyErr (List UF.Variable) ()


fmap : (a -> b) -> Unify a -> Unify b
fmap func (Unify kv) =
    Unify <|
        \vars ->
            IO.fmap
                (Result.map
                    (\(UnifyOk vars1 value) ->
                        UnifyOk vars1 (func value)
                    )
                )
                (kv vars)


pure : a -> Unify a
pure a =
    Unify (\vars -> IO.pure (Ok (UnifyOk vars a)))


bind : (a -> Unify b) -> Unify a -> Unify b
bind callback (Unify ka) =
    Unify <|
        \vars ->
            IO.bind
                (\result ->
                    case result of
                        Ok (UnifyOk vars1 a) ->
                            case callback a of
                                Unify kb ->
                                    kb vars1

                        Err err ->
                            IO.pure (Err err)
                )
                (ka vars)


register : IO UF.Variable -> Unify UF.Variable
register mkVar =
    Unify
        (\vars ->
            IO.fmap
                (\var ->
                    Ok (UnifyOk (var :: vars) var)
                )
                mkVar
        )


mismatch : Unify a
mismatch =
    Unify (\vars -> IO.pure (Err (UnifyErr vars ())))



-- UNIFICATION HELPERS


type Context
    = Context UF.Variable UF.Descriptor UF.Variable UF.Descriptor


reorient : Context -> Context
reorient (Context var1 desc1 var2 desc2) =
    Context var2 desc2 var1 desc1



-- MERGE
-- merge : Context -> UF.Content -> Unify ( UF.Point UF.Descriptor, UF.Point UF.Descriptor )


merge : Context -> UF.Content -> Unify ()
merge (Context var1 (UF.Descriptor _ rank1 _ _) var2 (UF.Descriptor _ rank2 _ _)) content =
    Unify
        (\vars ->
            UF.union var1 var2 (UF.Descriptor content (min rank1 rank2) Type.noMark Nothing)
                |> IO.fmap (Ok << UnifyOk vars)
        )


fresh : Context -> UF.Content -> Unify UF.Variable
fresh (Context _ (UF.Descriptor _ rank1 _ _) _ (UF.Descriptor _ rank2 _ _)) content =
    register <|
        UF.fresh <|
            UF.Descriptor content (min rank1 rank2) Type.noMark Nothing



-- ACTUALLY UNIFY THINGS


guardedUnify : UF.Variable -> UF.Variable -> Unify ()
guardedUnify left right =
    Unify
        (\vars ->
            UF.equivalent left right
                |> IO.bind
                    (\equivalent ->
                        if equivalent then
                            IO.pure (Ok (UnifyOk vars ()))

                        else
                            UF.get left
                                |> IO.bind
                                    (\leftDesc ->
                                        UF.get right
                                            |> IO.bind
                                                (\rightDesc ->
                                                    case actuallyUnify (Context left leftDesc right rightDesc) of
                                                        Unify k ->
                                                            k vars
                                                )
                                    )
                    )
        )


subUnify : UF.Variable -> UF.Variable -> Unify ()
subUnify var1 var2 =
    guardedUnify var1 var2


actuallyUnify : Context -> Unify ()
actuallyUnify ((Context _ (UF.Descriptor firstContent _ _ _) _ (UF.Descriptor secondContent _ _ _)) as context) =
    case firstContent of
        UF.FlexVar _ ->
            unifyFlex context firstContent secondContent

        UF.FlexSuper super _ ->
            unifyFlexSuper context super firstContent secondContent

        UF.RigidVar _ ->
            unifyRigid context Nothing firstContent secondContent

        UF.RigidSuper super _ ->
            unifyRigid context (Just super) firstContent secondContent

        UF.Alias home name args realVar ->
            unifyAlias context home name args realVar secondContent

        UF.Structure flatType ->
            unifyStructure context flatType firstContent secondContent

        UF.Error ->
            -- If there was an error, just pretend it is okay. This lets us avoid
            -- "cascading" errors where one problem manifests as multiple message.
            merge context UF.Error



-- UNIFY FLEXIBLE VARIABLES


unifyFlex : Context -> UF.Content -> UF.Content -> Unify ()
unifyFlex context content otherContent =
    case otherContent of
        UF.Error ->
            merge context UF.Error

        UF.FlexVar maybeName ->
            merge context <|
                case maybeName of
                    Nothing ->
                        content

                    Just _ ->
                        otherContent

        UF.FlexSuper _ _ ->
            merge context otherContent

        UF.RigidVar _ ->
            merge context otherContent

        UF.RigidSuper _ _ ->
            merge context otherContent

        UF.Alias _ _ _ _ ->
            merge context otherContent

        UF.Structure _ ->
            merge context otherContent



-- UNIFY RIGID VARIABLES


unifyRigid : Context -> Maybe UF.SuperType -> UF.Content -> UF.Content -> Unify ()
unifyRigid context maybeSuper content otherContent =
    case otherContent of
        UF.FlexVar _ ->
            merge context content

        UF.FlexSuper otherSuper _ ->
            case maybeSuper of
                Just super ->
                    if combineRigidSupers super otherSuper then
                        merge context content

                    else
                        mismatch

                Nothing ->
                    mismatch

        UF.RigidVar _ ->
            mismatch

        UF.RigidSuper _ _ ->
            mismatch

        UF.Alias _ _ _ _ ->
            mismatch

        UF.Structure _ ->
            mismatch

        UF.Error ->
            merge context UF.Error



-- UNIFY SUPER VARIABLES


unifyFlexSuper : Context -> UF.SuperType -> UF.Content -> UF.Content -> Unify ()
unifyFlexSuper ((Context first _ _ _) as context) super content otherContent =
    case otherContent of
        UF.Structure flatType ->
            unifyFlexSuperStructure context super flatType

        UF.RigidVar _ ->
            mismatch

        UF.RigidSuper otherSuper _ ->
            if combineRigidSupers otherSuper super then
                merge context otherContent

            else
                mismatch

        UF.FlexVar _ ->
            merge context content

        UF.FlexSuper otherSuper _ ->
            case super of
                UF.Number ->
                    case otherSuper of
                        UF.Number ->
                            merge context content

                        UF.Comparable ->
                            merge context content

                        UF.Appendable ->
                            mismatch

                        UF.CompAppend ->
                            mismatch

                UF.Comparable ->
                    case otherSuper of
                        UF.Comparable ->
                            merge context otherContent

                        UF.Number ->
                            merge context otherContent

                        UF.Appendable ->
                            merge context <| Type.unnamedFlexSuper UF.CompAppend

                        UF.CompAppend ->
                            merge context otherContent

                UF.Appendable ->
                    case otherSuper of
                        UF.Appendable ->
                            merge context otherContent

                        UF.Comparable ->
                            merge context <| Type.unnamedFlexSuper UF.CompAppend

                        UF.CompAppend ->
                            merge context otherContent

                        UF.Number ->
                            mismatch

                UF.CompAppend ->
                    case otherSuper of
                        UF.Comparable ->
                            merge context content

                        UF.Appendable ->
                            merge context content

                        UF.CompAppend ->
                            merge context content

                        UF.Number ->
                            mismatch

        UF.Alias _ _ _ realVar ->
            subUnify first realVar

        UF.Error ->
            merge context UF.Error


combineRigidSupers : UF.SuperType -> UF.SuperType -> Bool
combineRigidSupers rigid flex =
    rigid
        == flex
        || (rigid == UF.Number && flex == UF.Comparable)
        || (rigid == UF.CompAppend && (flex == UF.Comparable || flex == UF.Appendable))


atomMatchesSuper : UF.SuperType -> ModuleName.Canonical -> Name.Name -> Bool
atomMatchesSuper super home name =
    case super of
        UF.Number ->
            isNumber home name

        UF.Comparable ->
            isNumber home name || Error.isString home name || Error.isChar home name

        UF.Appendable ->
            Error.isString home name

        UF.CompAppend ->
            Error.isString home name


isNumber : ModuleName.Canonical -> Name.Name -> Bool
isNumber home name =
    (home == ModuleName.basics)
        && (name == Name.int || name == Name.float)


unifyFlexSuperStructure : Context -> UF.SuperType -> UF.FlatType -> Unify ()
unifyFlexSuperStructure context super flatType =
    case flatType of
        UF.App1 home name [] ->
            if atomMatchesSuper super home name then
                merge context (UF.Structure flatType)

            else
                mismatch

        UF.App1 home name [ variable ] ->
            if home == ModuleName.list && name == Name.list then
                case super of
                    UF.Number ->
                        mismatch

                    UF.Appendable ->
                        merge context (UF.Structure flatType)

                    UF.Comparable ->
                        comparableOccursCheck context
                            |> bind (\_ -> unifyComparableRecursive variable)
                            |> bind (\_ -> merge context (UF.Structure flatType))

                    UF.CompAppend ->
                        comparableOccursCheck context
                            |> bind (\_ -> unifyComparableRecursive variable)
                            |> bind (\_ -> merge context (UF.Structure flatType))

            else
                mismatch

        UF.Tuple1 a b maybeC ->
            case super of
                UF.Number ->
                    mismatch

                UF.Appendable ->
                    mismatch

                UF.Comparable ->
                    comparableOccursCheck context
                        |> bind (\_ -> unifyComparableRecursive a)
                        |> bind (\_ -> unifyComparableRecursive b)
                        |> bind
                            (\_ ->
                                case maybeC of
                                    Nothing ->
                                        pure ()

                                    Just c ->
                                        unifyComparableRecursive c
                            )
                        |> bind (\_ -> merge context (UF.Structure flatType))

                UF.CompAppend ->
                    mismatch

        _ ->
            mismatch



-- TODO: is there some way to avoid doing this?
-- Do type classes require occurs checks?


comparableOccursCheck : Context -> Unify ()
comparableOccursCheck (Context _ _ var _) =
    Unify
        (\vars ->
            Occurs.occurs var
                |> IO.fmap
                    (\hasOccurred ->
                        if hasOccurred then
                            Err (UnifyErr vars ())

                        else
                            Ok (UnifyOk vars ())
                    )
        )


unifyComparableRecursive : UF.Variable -> Unify ()
unifyComparableRecursive var =
    register
        (UF.get var
            |> IO.bind
                (\(UF.Descriptor _ rank _ _) ->
                    UF.fresh (UF.Descriptor (Type.unnamedFlexSuper UF.Comparable) rank Type.noMark Nothing)
                )
        )
        |> bind (\compVar -> guardedUnify compVar var)



-- UNIFY ALIASES


unifyAlias : Context -> ModuleName.Canonical -> Name.Name -> List ( Name.Name, UF.Variable ) -> UF.Variable -> UF.Content -> Unify ()
unifyAlias ((Context _ _ second _) as context) home name args realVar otherContent =
    case otherContent of
        UF.FlexVar _ ->
            merge context (UF.Alias home name args realVar)

        UF.FlexSuper _ _ ->
            subUnify realVar second

        UF.RigidVar _ ->
            subUnify realVar second

        UF.RigidSuper _ _ ->
            subUnify realVar second

        UF.Alias otherHome otherName otherArgs otherRealVar ->
            if name == otherName && home == otherHome then
                Unify
                    (\vars ->
                        unifyAliasArgs vars args otherArgs
                            |> IO.bind
                                (\res ->
                                    case res of
                                        Ok (UnifyOk vars1 ()) ->
                                            case merge context otherContent of
                                                Unify k ->
                                                    k vars1

                                        Err err ->
                                            IO.pure (Err err)
                                )
                    )

            else
                subUnify realVar otherRealVar

        UF.Structure _ ->
            subUnify realVar second

        UF.Error ->
            merge context UF.Error


unifyAliasArgs : List UF.Variable -> List ( Name.Name, UF.Variable ) -> List ( Name.Name, UF.Variable ) -> IO (Result UnifyErr (UnifyOk ()))
unifyAliasArgs vars args1 args2 =
    case args1 of
        ( _, arg1 ) :: others1 ->
            case args2 of
                ( _, arg2 ) :: others2 ->
                    case subUnify arg1 arg2 of
                        Unify k ->
                            k vars
                                |> IO.bind
                                    (\res1 ->
                                        case res1 of
                                            Ok (UnifyOk vs ()) ->
                                                unifyAliasArgs vs others1 others2

                                            Err (UnifyErr vs ()) ->
                                                unifyAliasArgs vs others1 others2
                                                    |> IO.fmap
                                                        (\res2 ->
                                                            case res2 of
                                                                Ok (UnifyOk vs_ ()) ->
                                                                    Err (UnifyErr vs_ ())

                                                                Err err ->
                                                                    Err err
                                                        )
                                    )

                _ ->
                    IO.pure (Err (UnifyErr vars ()))

        [] ->
            case args2 of
                [] ->
                    IO.pure (Ok (UnifyOk vars ()))

                _ ->
                    IO.pure (Err (UnifyErr vars ()))



-- UNIFY STRUCTURES


unifyStructure : Context -> UF.FlatType -> UF.Content -> UF.Content -> Unify ()
unifyStructure ((Context first _ second _) as context) flatType content otherContent =
    case otherContent of
        UF.FlexVar _ ->
            merge context content

        UF.FlexSuper super _ ->
            unifyFlexSuperStructure (reorient context) super flatType

        UF.RigidVar _ ->
            mismatch

        UF.RigidSuper _ _ ->
            mismatch

        UF.Alias _ _ _ realVar ->
            subUnify first realVar

        UF.Structure otherFlatType ->
            case ( flatType, otherFlatType ) of
                ( UF.App1 home name args, UF.App1 otherHome otherName otherArgs ) ->
                    if home == otherHome && name == otherName then
                        Unify
                            (\vars ->
                                unifyArgs vars args otherArgs
                                    |> IO.bind
                                        (\unifiedArgs ->
                                            case unifiedArgs of
                                                Ok (UnifyOk vars1 ()) ->
                                                    case merge context otherContent of
                                                        Unify k ->
                                                            k vars1

                                                Err err ->
                                                    IO.pure (Err err)
                                        )
                            )

                    else
                        mismatch

                ( UF.Fun1 arg1 res1, UF.Fun1 arg2 res2 ) ->
                    subUnify arg1 arg2
                        |> bind (\_ -> subUnify res1 res2)
                        |> bind (\_ -> merge context otherContent)

                ( UF.EmptyRecord1, UF.EmptyRecord1 ) ->
                    merge context otherContent

                ( UF.Record1 fields ext, UF.EmptyRecord1 ) ->
                    if Dict.isEmpty fields then
                        subUnify ext second

                    else
                        mismatch

                ( UF.EmptyRecord1, UF.Record1 fields ext ) ->
                    if Dict.isEmpty fields then
                        subUnify first ext

                    else
                        mismatch

                ( UF.Record1 fields1 ext1, UF.Record1 fields2 ext2 ) ->
                    Unify
                        (\vars ->
                            gatherFields fields1 ext1
                                |> IO.bind
                                    (\structure1 ->
                                        gatherFields fields2 ext2
                                            |> IO.bind
                                                (\structure2 ->
                                                    case unifyRecord context structure1 structure2 of
                                                        Unify k ->
                                                            k vars
                                                )
                                    )
                        )

                ( UF.Tuple1 a b Nothing, UF.Tuple1 x y Nothing ) ->
                    subUnify a x
                        |> bind (\_ -> subUnify b y)
                        |> bind (\_ -> merge context otherContent)

                ( UF.Tuple1 a b (Just c), UF.Tuple1 x y (Just z) ) ->
                    subUnify a x
                        |> bind (\_ -> subUnify b y)
                        |> bind (\_ -> subUnify c z)
                        |> bind (\_ -> merge context otherContent)

                ( UF.Unit1, UF.Unit1 ) ->
                    merge context otherContent

                _ ->
                    mismatch

        UF.Error ->
            merge context UF.Error



-- UNIFY ARGS


unifyArgs : List UF.Variable -> List UF.Variable -> List UF.Variable -> IO (Result UnifyErr (UnifyOk ()))
unifyArgs vars args1 args2 =
    case args1 of
        arg1 :: others1 ->
            case args2 of
                arg2 :: others2 ->
                    case subUnify arg1 arg2 of
                        Unify k ->
                            k vars
                                |> IO.bind
                                    (\result ->
                                        case result of
                                            Ok (UnifyOk vs ()) ->
                                                unifyArgs vs others1 others2

                                            Err (UnifyErr vs ()) ->
                                                unifyArgs vs others1 others2
                                                    |> IO.fmap
                                                        (Result.andThen
                                                            (\(UnifyOk vs_ ()) ->
                                                                Err (UnifyErr vs_ ())
                                                            )
                                                        )
                                    )

                _ ->
                    IO.pure (Err (UnifyErr vars ()))

        [] ->
            case args2 of
                [] ->
                    IO.pure (Ok (UnifyOk vars ()))

                _ ->
                    IO.pure (Err (UnifyErr vars ()))



-- UNIFY RECORDS


unifyRecord : Context -> RecordStructure -> RecordStructure -> Unify ()
unifyRecord context (RecordStructure fields1 ext1) (RecordStructure fields2 ext2) =
    let
        sharedFields : Dict Name.Name ( UF.Variable, UF.Variable )
        sharedFields =
            Utils.mapIntersectionWith compare Tuple.pair fields1 fields2

        uniqueFields1 : Dict Name.Name UF.Variable
        uniqueFields1 =
            Dict.diff fields1 fields2

        uniqueFields2 : Dict Name.Name UF.Variable
        uniqueFields2 =
            Dict.diff fields2 fields1
    in
    if Dict.isEmpty uniqueFields1 then
        if Dict.isEmpty uniqueFields2 then
            subUnify ext1 ext2
                |> bind (\_ -> unifySharedFields context sharedFields Dict.empty ext1)

        else
            fresh context (UF.Structure (UF.Record1 uniqueFields2 ext2))
                |> bind
                    (\subRecord ->
                        subUnify ext1 subRecord
                            |> bind (\_ -> unifySharedFields context sharedFields Dict.empty subRecord)
                    )

    else if Dict.isEmpty uniqueFields2 then
        fresh context (UF.Structure (UF.Record1 uniqueFields1 ext1))
            |> bind
                (\subRecord ->
                    subUnify subRecord ext2
                        |> bind (\_ -> unifySharedFields context sharedFields Dict.empty subRecord)
                )

    else
        let
            otherFields : Dict Name.Name UF.Variable
            otherFields =
                Dict.union compare uniqueFields1 uniqueFields2
        in
        fresh context Type.unnamedFlexVar
            |> bind
                (\ext ->
                    fresh context (UF.Structure (UF.Record1 uniqueFields1 ext))
                        |> bind
                            (\sub1 ->
                                fresh context (UF.Structure (UF.Record1 uniqueFields2 ext))
                                    |> bind
                                        (\sub2 ->
                                            subUnify ext1 sub2
                                                |> bind (\_ -> subUnify sub1 ext2)
                                                |> bind (\_ -> unifySharedFields context sharedFields otherFields ext)
                                        )
                            )
                )


unifySharedFields : Context -> Dict Name.Name ( UF.Variable, UF.Variable ) -> Dict Name.Name UF.Variable -> UF.Variable -> Unify ()
unifySharedFields context sharedFields otherFields ext =
    traverseMaybe compare unifyField sharedFields
        |> bind
            (\matchingFields ->
                if Dict.size sharedFields == Dict.size matchingFields then
                    merge context (UF.Structure (UF.Record1 (Dict.union compare matchingFields otherFields) ext))

                else
                    mismatch
            )


traverseMaybe : (a -> a -> Order) -> (a -> b -> Unify (Maybe c)) -> Dict a b -> Unify (Dict a c)
traverseMaybe keyComparison func =
    Dict.foldl
        (\a b ->
            bind
                (\acc ->
                    fmap
                        (\maybeC ->
                            maybeC
                                |> Maybe.map (\c -> Dict.insert keyComparison a c acc)
                                |> Maybe.withDefault acc
                        )
                        (func a b)
                )
        )
        (pure Dict.empty)


unifyField : Name.Name -> ( UF.Variable, UF.Variable ) -> Unify (Maybe UF.Variable)
unifyField _ ( actual, expected ) =
    Unify
        (\vars ->
            case subUnify actual expected of
                Unify k ->
                    k vars
                        |> IO.fmap
                            (\result ->
                                case result of
                                    Ok (UnifyOk vs ()) ->
                                        Ok (UnifyOk vs (Just actual))

                                    Err (UnifyErr vs ()) ->
                                        Ok (UnifyOk vs Nothing)
                            )
        )



-- GATHER RECORD STRUCTURE


type RecordStructure
    = RecordStructure (Dict Name.Name UF.Variable) UF.Variable


gatherFields : Dict Name.Name UF.Variable -> UF.Variable -> IO RecordStructure
gatherFields fields variable =
    UF.get variable
        |> IO.bind
            (\(UF.Descriptor content _ _ _) ->
                case content of
                    UF.Structure (UF.Record1 subFields subExt) ->
                        gatherFields (Dict.union compare fields subFields) subExt

                    UF.Alias _ _ _ var ->
                        -- TODO may be dropping useful alias info here
                        gatherFields fields var

                    _ ->
                        IO.pure (RecordStructure fields variable)
            )
