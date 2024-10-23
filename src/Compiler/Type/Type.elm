module Compiler.Type.Type exposing
    ( Constraint(..)
    , Type(..)
    , bool
    , char
    , exists
    , float
    , funType
    , int
    , mat4
    , mkFlexNumber
    , mkFlexVar
    , nameToFlex
    , nameToRigid
    , never
    , nextMark
    , noMark
    , noRank
    , outermostRank
    , string
    , texture
    , toAnnotation
    , toErrorType
    , unnamedFlexSuper
    , unnamedFlexVar
    , vec2
    , vec3
    , vec4
    )

import Compiler.AST.Canonical as Can
import Compiler.AST.Utils.Type as Type
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Type as E
import Compiler.Type.Error as ET
import Compiler.Type.UnionFind as UF
import Data.IO as IO exposing (IO)
import Data.Map as Dict exposing (Dict)
import Data.Maybe as Maybe
import Utils.Crash exposing (crash)
import Utils.Main as Utils



-- CONSTRAINTS


type Constraint
    = CTrue
    | CSaveTheEnvironment
    | CEqual A.Region E.Category Type (E.Expected Type)
    | CLocal A.Region Name (E.Expected Type)
    | CForeign A.Region Name Can.Annotation (E.Expected Type)
    | CPattern A.Region E.PCategory Type (E.PExpected Type)
    | CAnd (List Constraint)
    | CLet (List UF.Variable) (List UF.Variable) (Dict Name (A.Located Type)) Constraint Constraint


exists : List UF.Variable -> Constraint -> Constraint
exists flexVars constraint =
    CLet [] flexVars Dict.empty constraint CTrue



-- TYPE PRIMITIVES


type Type
    = PlaceHolder Name
    | AliasN ModuleName.Canonical Name (List ( Name, Type )) Type
    | VarN UF.Variable
    | AppN ModuleName.Canonical Name (List Type)
    | FunN Type Type
    | EmptyRecordN
    | RecordN (Dict Name Type) Type
    | UnitN
    | TupleN Type Type (Maybe Type)



-- DESCRIPTORS


makeDescriptor : UF.Content -> UF.Descriptor
makeDescriptor content =
    UF.Descriptor content noRank noMark Nothing



-- RANKS


noRank : Int
noRank =
    0


outermostRank : Int
outermostRank =
    1



-- MARKS


noMark : UF.Mark
noMark =
    UF.Mark 2


occursMark : UF.Mark
occursMark =
    UF.Mark 1


getVarNamesMark : UF.Mark
getVarNamesMark =
    UF.Mark 0


nextMark : UF.Mark -> UF.Mark
nextMark (UF.Mark mark) =
    UF.Mark (mark + 1)



-- FUNCTION TYPES


funType : Type -> Type -> Type
funType =
    FunN



-- PRIMITIVE TYPES


int : Type
int =
    AppN ModuleName.basics "Int" []


float : Type
float =
    AppN ModuleName.basics "Float" []


char : Type
char =
    AppN ModuleName.char "Char" []


string : Type
string =
    AppN ModuleName.string "String" []


bool : Type
bool =
    AppN ModuleName.basics "Bool" []


never : Type
never =
    AppN ModuleName.basics "Never" []



-- WEBGL TYPES


vec2 : Type
vec2 =
    AppN ModuleName.vector2 "Vec2" []


vec3 : Type
vec3 =
    AppN ModuleName.vector3 "Vec3" []


vec4 : Type
vec4 =
    AppN ModuleName.vector4 "Vec4" []


mat4 : Type
mat4 =
    AppN ModuleName.matrix4 "Mat4" []


texture : Type
texture =
    AppN ModuleName.texture "Texture" []



-- MAKE FLEX VARIABLES


mkFlexVar : IO UF.Variable
mkFlexVar =
    UF.fresh flexVarDescriptor


flexVarDescriptor : UF.Descriptor
flexVarDescriptor =
    makeDescriptor unnamedFlexVar


unnamedFlexVar : UF.Content
unnamedFlexVar =
    UF.FlexVar Nothing



-- MAKE FLEX NUMBERS


mkFlexNumber : IO UF.Variable
mkFlexNumber =
    UF.fresh flexNumberDescriptor


flexNumberDescriptor : UF.Descriptor
flexNumberDescriptor =
    makeDescriptor (unnamedFlexSuper UF.Number)


unnamedFlexSuper : UF.SuperType -> UF.Content
unnamedFlexSuper super =
    UF.FlexSuper super Nothing



-- MAKE NAMED VARIABLES


nameToFlex : Name -> IO UF.Variable
nameToFlex name =
    UF.fresh <|
        makeDescriptor <|
            Maybe.maybe UF.FlexVar UF.FlexSuper (toSuper name) (Just name)


nameToRigid : Name -> IO UF.Variable
nameToRigid name =
    UF.fresh <|
        makeDescriptor <|
            Maybe.maybe UF.RigidVar UF.RigidSuper (toSuper name) name


toSuper : Name -> Maybe UF.SuperType
toSuper name =
    if Name.isNumberType name then
        Just UF.Number

    else if Name.isComparableType name then
        Just UF.Comparable

    else if Name.isAppendableType name then
        Just UF.Appendable

    else if Name.isCompappendType name then
        Just UF.CompAppend

    else
        Nothing



-- TO TYPE ANNOTATION


toAnnotation : UF.Variable -> IO Can.Annotation
toAnnotation variable =
    getVarNames variable Dict.empty
        |> IO.bind
            (\userNames ->
                IO.runStateT (variableToCanType variable) (makeNameState userNames)
                    |> IO.fmap
                        (\( tipe, NameState freeVars _ _ _ _ _ ) ->
                            Can.Forall freeVars tipe
                        )
            )


variableToCanType : UF.Variable -> IO.StateT NameState Can.Type
variableToCanType variable =
    IO.liftIO (UF.get variable)
        |> IO.bindStateT
            (\(UF.Descriptor content _ _ _) ->
                case content of
                    UF.Structure term ->
                        termToCanType term

                    UF.FlexVar maybeName ->
                        case maybeName of
                            Just name ->
                                IO.pureStateT (Can.TVar name)

                            Nothing ->
                                getFreshVarName
                                    |> IO.bindStateT
                                        (\name ->
                                            IO.liftIO
                                                (UF.modify variable
                                                    (\(UF.Descriptor _ rank mark copy) ->
                                                        UF.Descriptor (UF.FlexVar (Just name)) rank mark copy
                                                    )
                                                )
                                                |> IO.fmapStateT (\_ -> Can.TVar name)
                                        )

                    UF.FlexSuper super maybeName ->
                        case maybeName of
                            Just name ->
                                IO.pureStateT (Can.TVar name)

                            Nothing ->
                                getFreshSuperName super
                                    |> IO.bindStateT
                                        (\name ->
                                            IO.liftIO
                                                (UF.modify variable
                                                    (\(UF.Descriptor _ rank mark copy) ->
                                                        UF.Descriptor (UF.FlexSuper super (Just name)) rank mark copy
                                                    )
                                                )
                                                |> IO.fmapStateT (\_ -> Can.TVar name)
                                        )

                    UF.RigidVar name ->
                        IO.pureStateT (Can.TVar name)

                    UF.RigidSuper _ name ->
                        IO.pureStateT (Can.TVar name)

                    UF.Alias home name args realVariable ->
                        Utils.listTraverseStateT (Utils.tupleTraverseStateT variableToCanType) args
                            |> IO.bindStateT
                                (\canArgs ->
                                    variableToCanType realVariable
                                        |> IO.fmapStateT
                                            (\canType ->
                                                Can.TAlias home name canArgs (Can.Filled canType)
                                            )
                                )

                    UF.Error ->
                        crash "cannot handle Error types in variableToCanType"
            )


termToCanType : UF.FlatType -> IO.StateT NameState Can.Type
termToCanType term =
    case term of
        UF.App1 home name args ->
            Utils.listTraverseStateT variableToCanType args
                |> IO.fmapStateT (Can.TType home name)

        UF.Fun1 a b ->
            IO.pureStateT Can.TLambda
                |> IO.applyStateT (variableToCanType a)
                |> IO.applyStateT (variableToCanType b)

        UF.EmptyRecord1 ->
            IO.pureStateT (Can.TRecord Dict.empty Nothing)

        UF.Record1 fields extension ->
            Utils.mapTraverseStateT compare fieldToCanType fields
                |> IO.bindStateT
                    (\canFields ->
                        variableToCanType extension
                            |> IO.fmapStateT Type.iteratedDealias
                            |> IO.fmapStateT
                                (\canExt ->
                                    case canExt of
                                        Can.TRecord subFields subExt ->
                                            Can.TRecord (Dict.union compare subFields canFields) subExt

                                        Can.TVar name ->
                                            Can.TRecord canFields (Just name)

                                        _ ->
                                            crash "Used toAnnotation on a type that is not well-formed"
                                )
                    )

        UF.Unit1 ->
            IO.pureStateT Can.TUnit

        UF.Tuple1 a b maybeC ->
            IO.pureStateT Can.TTuple
                |> IO.applyStateT (variableToCanType a)
                |> IO.applyStateT (variableToCanType b)
                |> IO.applyStateT (Utils.maybeTraverseStateT variableToCanType maybeC)


fieldToCanType : UF.Variable -> IO.StateT NameState Can.FieldType
fieldToCanType variable =
    variableToCanType variable
        |> IO.fmapStateT (\tipe -> Can.FieldType 0 tipe)



-- TO ERROR TYPE


toErrorType : UF.Variable -> IO ET.Type
toErrorType variable =
    getVarNames variable Dict.empty
        |> IO.bind
            (\userNames ->
                IO.evalStateT (variableToErrorType variable) (makeNameState userNames)
            )


variableToErrorType : UF.Variable -> IO.StateT NameState ET.Type
variableToErrorType variable =
    IO.liftIO (UF.get variable)
        |> IO.bindStateT
            (\(UF.Descriptor content _ mark _) ->
                if mark == occursMark then
                    IO.pureStateT ET.Infinite

                else
                    IO.liftIO (UF.modify variable (\(UF.Descriptor content_ rank_ _ copy_) -> UF.Descriptor content_ rank_ occursMark copy_))
                        |> IO.bindStateT
                            (\_ ->
                                contentToErrorType variable content
                                    |> IO.bindStateT
                                        (\errType ->
                                            IO.liftIO (UF.modify variable (\(UF.Descriptor content_ rank_ _ copy_) -> UF.Descriptor content_ rank_ mark copy_))
                                                |> IO.fmapStateT (\_ -> errType)
                                        )
                            )
            )


contentToErrorType : UF.Variable -> UF.Content -> IO.StateT NameState ET.Type
contentToErrorType variable content =
    case content of
        UF.Structure term ->
            termToErrorType term

        UF.FlexVar maybeName ->
            case maybeName of
                Just name ->
                    IO.pureStateT (ET.FlexVar name)

                Nothing ->
                    getFreshVarName
                        |> IO.bindStateT
                            (\name ->
                                IO.liftIO
                                    (UF.modify variable
                                        (\(UF.Descriptor _ rank mark copy) ->
                                            UF.Descriptor (UF.FlexVar (Just name)) rank mark copy
                                        )
                                    )
                                    |> IO.fmapStateT (\_ -> ET.FlexVar name)
                            )

        UF.FlexSuper super maybeName ->
            case maybeName of
                Just name ->
                    IO.pureStateT (ET.FlexSuper (superToSuper super) name)

                Nothing ->
                    getFreshSuperName super
                        |> IO.bindStateT
                            (\name ->
                                IO.liftIO
                                    (UF.modify variable
                                        (\(UF.Descriptor _ rank mark copy) ->
                                            UF.Descriptor (UF.FlexSuper super (Just name)) rank mark copy
                                        )
                                    )
                                    |> IO.fmapStateT (\_ -> ET.FlexSuper (superToSuper super) name)
                            )

        UF.RigidVar name ->
            IO.pureStateT (ET.RigidVar name)

        UF.RigidSuper super name ->
            IO.pureStateT (ET.RigidSuper (superToSuper super) name)

        UF.Alias home name args realVariable ->
            Utils.listTraverseStateT (Utils.tupleTraverseStateT variableToErrorType) args
                |> IO.bindStateT
                    (\errArgs ->
                        variableToErrorType realVariable
                            |> IO.fmapStateT
                                (\errType ->
                                    ET.Alias home name errArgs errType
                                )
                    )

        UF.Error ->
            IO.pureStateT ET.Error


superToSuper : UF.SuperType -> ET.Super
superToSuper super =
    case super of
        UF.Number ->
            ET.Number

        UF.Comparable ->
            ET.Comparable

        UF.Appendable ->
            ET.Appendable

        UF.CompAppend ->
            ET.CompAppend


termToErrorType : UF.FlatType -> IO.StateT NameState ET.Type
termToErrorType term =
    case term of
        UF.App1 home name args ->
            Utils.listTraverseStateT variableToErrorType args
                |> IO.fmapStateT (ET.Type home name)

        UF.Fun1 a b ->
            variableToErrorType a
                |> IO.bindStateT
                    (\arg ->
                        variableToErrorType b
                            |> IO.fmapStateT
                                (\result ->
                                    case result of
                                        ET.Lambda arg1 arg2 others ->
                                            ET.Lambda arg arg1 (arg2 :: others)

                                        _ ->
                                            ET.Lambda arg result []
                                )
                    )

        UF.EmptyRecord1 ->
            IO.pureStateT (ET.Record Dict.empty ET.Closed)

        UF.Record1 fields extension ->
            Utils.mapTraverseStateT compare variableToErrorType fields
                |> IO.bindStateT
                    (\errFields ->
                        variableToErrorType extension
                            |> IO.fmapStateT ET.iteratedDealias
                            |> IO.fmapStateT
                                (\errExt ->
                                    case errExt of
                                        ET.Record subFields subExt ->
                                            ET.Record (Dict.union compare subFields errFields) subExt

                                        ET.FlexVar ext ->
                                            ET.Record errFields (ET.FlexOpen ext)

                                        ET.RigidVar ext ->
                                            ET.Record errFields (ET.RigidOpen ext)

                                        _ ->
                                            crash "Used toErrorType on a type that is not well-formed"
                                )
                    )

        UF.Unit1 ->
            IO.pureStateT ET.Unit

        UF.Tuple1 a b maybeC ->
            IO.pureStateT ET.Tuple
                |> IO.applyStateT (variableToErrorType a)
                |> IO.applyStateT (variableToErrorType b)
                |> IO.applyStateT (Utils.maybeTraverseStateT variableToErrorType maybeC)



-- MANAGE FRESH VARIABLE NAMES


type NameState
    = NameState (Dict Name ()) Int Int Int Int Int


makeNameState : Dict Name UF.Variable -> NameState
makeNameState taken =
    NameState (Dict.map (\_ _ -> ()) taken) 0 0 0 0 0



-- FRESH VAR NAMES


getFreshVarName : IO.StateT NameState Name
getFreshVarName =
    IO.gets (\(NameState _ normals _ _ _ _) -> normals)
        |> IO.bindStateT
            (\index ->
                IO.gets (\(NameState taken _ _ _ _ _) -> taken)
                    |> IO.bindStateT
                        (\taken ->
                            let
                                ( name, newIndex, newTaken ) =
                                    getFreshVarNameHelp index taken
                            in
                            IO.modify
                                (\(NameState _ _ numbers comparables appendables compAppends) ->
                                    NameState newTaken newIndex numbers comparables appendables compAppends
                                )
                                |> IO.fmapStateT (\_ -> name)
                        )
            )


getFreshVarNameHelp : Int -> Dict Name () -> ( Name, Int, Dict Name () )
getFreshVarNameHelp index taken =
    let
        name : Name
        name =
            Name.fromTypeVariableScheme index
    in
    if Dict.member name taken then
        getFreshVarNameHelp (index + 1) taken

    else
        ( name, index + 1, Dict.insert compare name () taken )



-- FRESH SUPER NAMES


getFreshSuperName : UF.SuperType -> IO.StateT NameState Name
getFreshSuperName super =
    case super of
        UF.Number ->
            getFreshSuper "number"
                (\(NameState _ _ numbers _ _ _) -> numbers)
                (\index (NameState taken normals _ comparables appendables compAppends) ->
                    NameState taken normals index comparables appendables compAppends
                )

        UF.Comparable ->
            getFreshSuper "comparable"
                (\(NameState _ _ _ comparables _ _) -> comparables)
                (\index (NameState taken normals numbers _ appendables compAppends) ->
                    NameState taken normals numbers index appendables compAppends
                )

        UF.Appendable ->
            getFreshSuper "appendable"
                (\(NameState _ _ _ _ appendables _) -> appendables)
                (\index (NameState taken normals numbers comparables _ compAppends) ->
                    NameState taken normals numbers comparables index compAppends
                )

        UF.CompAppend ->
            getFreshSuper "compappend"
                (\(NameState _ _ _ _ _ compAppends) -> compAppends)
                (\index (NameState taken normals numbers comparables appendables _) ->
                    NameState taken normals numbers comparables appendables index
                )


getFreshSuper : Name -> (NameState -> Int) -> (Int -> NameState -> NameState) -> IO.StateT NameState Name
getFreshSuper prefix getter setter =
    IO.gets getter
        |> IO.bindStateT
            (\index ->
                IO.gets (\(NameState taken _ _ _ _ _) -> taken)
                    |> IO.bindStateT
                        (\taken ->
                            let
                                ( name, newIndex, newTaken ) =
                                    getFreshSuperHelp prefix index taken
                            in
                            IO.modify
                                (\(NameState _ normals numbers comparables appendables compAppends) ->
                                    setter newIndex (NameState newTaken normals numbers comparables appendables compAppends)
                                )
                                |> IO.fmapStateT (\_ -> name)
                        )
            )


getFreshSuperHelp : Name -> Int -> Dict Name () -> ( Name, Int, Dict Name () )
getFreshSuperHelp prefix index taken =
    let
        name : Name
        name =
            Name.fromTypeVariable prefix index
    in
    if Dict.member name taken then
        getFreshSuperHelp prefix (index + 1) taken

    else
        ( name, index + 1, Dict.insert compare name () taken )



-- GET ALL VARIABLE NAMES


getVarNames : UF.Variable -> Dict Name UF.Variable -> IO (Dict Name UF.Variable)
getVarNames var takenNames =
    UF.get var
        |> IO.bind
            (\(UF.Descriptor content rank mark copy) ->
                if mark == getVarNamesMark then
                    IO.pure takenNames

                else
                    UF.set var (UF.Descriptor content rank getVarNamesMark copy)
                        |> IO.bind
                            (\_ ->
                                case content of
                                    UF.Error ->
                                        IO.pure takenNames

                                    UF.FlexVar maybeName ->
                                        case maybeName of
                                            Nothing ->
                                                IO.pure takenNames

                                            Just name ->
                                                addName 0 name var (UF.FlexVar << Just) takenNames

                                    UF.FlexSuper super maybeName ->
                                        case maybeName of
                                            Nothing ->
                                                IO.pure takenNames

                                            Just name ->
                                                addName 0 name var (UF.FlexSuper super << Just) takenNames

                                    UF.RigidVar name ->
                                        addName 0 name var UF.RigidVar takenNames

                                    UF.RigidSuper super name ->
                                        addName 0 name var (UF.RigidSuper super) takenNames

                                    UF.Alias _ _ args _ ->
                                        Utils.ioFoldrM getVarNames takenNames (List.map Tuple.second args)

                                    UF.Structure flatType ->
                                        case flatType of
                                            UF.App1 _ _ args ->
                                                Utils.ioFoldrM getVarNames takenNames args

                                            UF.Fun1 arg body ->
                                                IO.bind (getVarNames arg) (getVarNames body takenNames)

                                            UF.EmptyRecord1 ->
                                                IO.pure takenNames

                                            UF.Record1 fields extension ->
                                                IO.bind (getVarNames extension)
                                                    (Utils.ioFoldrM getVarNames takenNames (Dict.values fields))

                                            UF.Unit1 ->
                                                IO.pure takenNames

                                            UF.Tuple1 a b Nothing ->
                                                IO.bind (getVarNames a) (getVarNames b takenNames)

                                            UF.Tuple1 a b (Just c) ->
                                                getVarNames c takenNames
                                                    |> IO.bind (getVarNames b)
                                                    |> IO.bind (getVarNames a)
                            )
            )



-- REGISTER NAME / RENAME DUPLICATES


addName : Int -> Name -> UF.Variable -> (Name -> UF.Content) -> Dict Name UF.Variable -> IO (Dict Name UF.Variable)
addName index givenName var makeContent takenNames =
    let
        indexedName : Name
        indexedName =
            Name.fromTypeVariable givenName index
    in
    case Dict.get indexedName takenNames of
        Nothing ->
            (if indexedName == givenName then
                IO.pure ()

             else
                UF.modify var
                    (\(UF.Descriptor _ rank mark copy) ->
                        UF.Descriptor (makeContent indexedName) rank mark copy
                    )
            )
                |> IO.fmap (\_ -> Dict.insert compare indexedName var takenNames)

        Just otherVar ->
            UF.equivalent var otherVar
                |> IO.bind
                    (\same ->
                        if same then
                            IO.pure takenNames

                        else
                            addName (index + 1) givenName var makeContent takenNames
                    )
