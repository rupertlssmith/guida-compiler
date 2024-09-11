module Type.UnionFind exposing
    ( Content(..)
    , Descriptor(..)
    , FlatType(..)
    , Mark(..)
    , Point
    , SuperType(..)
    , Variable
    , equivalent
    , fresh
    , get
    , modify
    , redundant
    , set
    , union
    , variableDecoder
    , variableEncoder
    )

{- This is based on the following implementations:

     - https://hackage.haskell.org/package/union-find-0.2/docs/src/Data-UnionFind-IO.html
     - http://yann.regis-gianas.org/public/mini/code_UnionFind.html

   It seems like the OCaml one came first, but I am not sure.

   Compared to the Haskell implementation, the major changes here include:

     1. No more reallocating PointInfo when changing the weight
     2. Using the strict modifyIORef

-}

import Data.IO as IO exposing (IO, IORef)
import Data.Map as Dict exposing (Dict)
import Data.Name exposing (Name)
import Elm.ModuleName as ModuleName
import Json.Decode as Decode
import Json.DecodeX as D
import Json.Encode as Encode
import Utils.Crash exposing (crash)



-- FROM TYPE


type Descriptor
    = Descriptor Content Int Mark (Maybe Variable)


descriptorEncoder : Descriptor -> Encode.Value
descriptorEncoder (Descriptor content rank mark copy) =
    Encode.object
        [ ( "type", Encode.string "Descriptor" )
        , ( "content", contentEncoder content )
        , ( "rank", Encode.int rank )
        , ( "mark", markEncoder mark )
        , ( "copy"
          , copy
                |> Maybe.map variableEncoder
                |> Maybe.withDefault Encode.null
          )
        ]


descriptorDecoder : Decode.Decoder Descriptor
descriptorDecoder =
    Decode.map4 Descriptor
        (Decode.field "content" contentDecoder)
        (Decode.field "rank" Decode.int)
        (Decode.field "mark" markDecoder)
        (Decode.field "copy" (Decode.maybe variableDecoder))


type Content
    = FlexVar (Maybe Name)
    | FlexSuper SuperType (Maybe Name)
    | RigidVar Name
    | RigidSuper SuperType Name
    | Structure FlatType
    | Alias ModuleName.Canonical Name (List ( Name, Variable )) Variable
    | Error


contentEncoder : Content -> Encode.Value
contentEncoder content =
    case content of
        FlexVar maybeName ->
            Encode.object
                [ ( "type", Encode.string "FlexVar" )
                , ( "name"
                  , maybeName
                        |> Maybe.map Encode.string
                        |> Maybe.withDefault Encode.null
                  )
                ]

        FlexSuper superType maybeName ->
            Encode.object
                [ ( "type", Encode.string "FlexSuper" )
                , ( "superType", superTypeEncoder superType )
                , ( "name"
                  , maybeName
                        |> Maybe.map Encode.string
                        |> Maybe.withDefault Encode.null
                  )
                ]

        RigidVar name ->
            Encode.object
                [ ( "type", Encode.string "RigidVar" )
                , ( "name", Encode.string name )
                ]

        RigidSuper superType name ->
            Encode.object
                [ ( "type", Encode.string "RigidSuper" )
                , ( "superType", superTypeEncoder superType )
                , ( "name", Encode.string name )
                ]

        Structure flatType ->
            Encode.object
                [ ( "type", Encode.string "Structure" )
                , ( "flatType", flatTypeEncoder flatType )
                ]

        Alias canonical name variableList variable ->
            Encode.object
                [ ( "type", Encode.string "Alias" )
                , ( "canonical", ModuleName.canonicalEncoder canonical )
                , ( "name", Encode.string name )
                , ( "variableList", Encode.object (List.map (Tuple.mapSecond variableEncoder) variableList) )
                , ( "variable", variableEncoder variable )
                ]

        Error ->
            Encode.object
                [ ( "type", Encode.string "Error" )
                ]


contentDecoder : Decode.Decoder Content
contentDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "FlexVar" ->
                        Decode.map FlexVar
                            (Decode.field "name" (Decode.maybe Decode.string))

                    "FlexSuper" ->
                        Decode.map2 FlexSuper
                            (Decode.field "superType" superTypeDecoder)
                            (Decode.field "name" (Decode.maybe Decode.string))

                    "RigidVar" ->
                        Decode.map RigidVar
                            (Decode.field "name" Decode.string)

                    "RigidSuper" ->
                        Decode.map2 RigidSuper
                            (Decode.field "superType" superTypeDecoder)
                            (Decode.field "name" Decode.string)

                    "Structure" ->
                        Decode.map Structure
                            (Decode.field "flatType" flatTypeDecoder)

                    "Alias" ->
                        Decode.map4 Alias
                            (Decode.field "canonical" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "variableList" (Decode.keyValuePairs variableDecoder))
                            (Decode.field "variable" variableDecoder)

                    "Error" ->
                        Decode.succeed Error

                    _ ->
                        Decode.fail ("Unknown Content's type: " ++ type_)
            )


type SuperType
    = Number
    | Comparable
    | Appendable
    | CompAppend


superTypeEncoder : SuperType -> Encode.Value
superTypeEncoder superType =
    case superType of
        Number ->
            Encode.string "Number"

        Comparable ->
            Encode.string "Comparable"

        Appendable ->
            Encode.string "Appendable"

        CompAppend ->
            Encode.string "CompAppend"


superTypeDecoder : Decode.Decoder SuperType
superTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Number" ->
                        Decode.succeed Number

                    "Comparable" ->
                        Decode.succeed Comparable

                    "Appendable" ->
                        Decode.succeed Appendable

                    "CompAppend" ->
                        Decode.succeed CompAppend

                    _ ->
                        Decode.fail ("Failed to decode SuperType: " ++ str)
            )


type FlatType
    = App1 ModuleName.Canonical Name (List Variable)
    | Fun1 Variable Variable
    | EmptyRecord1
    | Record1 (Dict Name Variable) Variable
    | Unit1
    | Tuple1 Variable Variable (Maybe Variable)


flatTypeEncoder : FlatType -> Encode.Value
flatTypeEncoder flatType =
    case flatType of
        App1 canonical name variableList ->
            Encode.object
                [ ( "type", Encode.string "App1" )
                , ( "canonical", ModuleName.canonicalEncoder canonical )
                , ( "name", Encode.string name )
                , ( "variableList", Encode.list variableEncoder variableList )
                ]

        Fun1 var1 var2 ->
            Encode.object
                [ ( "type", Encode.string "Fun1" )
                , ( "var1", variableEncoder var1 )
                , ( "var2", variableEncoder var2 )
                ]

        EmptyRecord1 ->
            Encode.object
                [ ( "type", Encode.string "EmptyRecord1" )
                ]

        Record1 variableDict variable ->
            Encode.object
                [ ( "type", Encode.string "Record1" )
                , ( "variableDict"
                  , Dict.toList variableDict
                        |> Encode.list
                            (\( name, var ) ->
                                Encode.object
                                    [ ( "a", Encode.string name )
                                    , ( "b", variableEncoder var )
                                    ]
                            )
                  )
                , ( "variable", variableEncoder variable )
                ]

        Unit1 ->
            Encode.object
                [ ( "type", Encode.string "Unit1" )
                ]

        Tuple1 var1 var2 maybeVariable ->
            Encode.object
                [ ( "type", Encode.string "Tuple1" )
                , ( "var1", variableEncoder var1 )
                , ( "var2", variableEncoder var2 )
                , ( "maybeVariable"
                  , maybeVariable
                        |> Maybe.map variableEncoder
                        |> Maybe.withDefault Encode.null
                  )
                ]


flatTypeDecoder : Decode.Decoder FlatType
flatTypeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "App1" ->
                        Decode.map3 App1
                            (Decode.field "canonical" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "variableList" (Decode.list variableDecoder))

                    "Fun1" ->
                        Decode.map2 Fun1
                            (Decode.field "var1" variableDecoder)
                            (Decode.field "var2" variableDecoder)

                    "EmptyRecord1" ->
                        Decode.succeed EmptyRecord1

                    "Record1" ->
                        Decode.map2 Record1
                            (Decode.field "variableDict" (D.assocListDict compare Decode.string variableDecoder))
                            (Decode.field "variable" variableDecoder)

                    "Unit1" ->
                        Decode.succeed Unit1

                    "Tuple1" ->
                        Decode.map3 Tuple1
                            (Decode.field "var1" variableDecoder)
                            (Decode.field "var2" variableDecoder)
                            (Decode.field "maybeVariable" (Decode.maybe variableDecoder))

                    _ ->
                        Decode.fail ("Unknown FlatType's type: " ++ type_)
            )


type Mark
    = Mark Int


markEncoder : Mark -> Encode.Value
markEncoder (Mark value) =
    Encode.int value


markDecoder : Decode.Decoder Mark
markDecoder =
    Decode.map Mark Decode.int


type alias Variable =
    Point


variableEncoder : Variable -> Encode.Value
variableEncoder =
    pointEncoder


variableDecoder : Decode.Decoder Variable
variableDecoder =
    pointDecoder



-- POINT


type Point
    = Pt (IORef PointInfo)


pointEncoder : Point -> Encode.Value
pointEncoder (Pt ioRef) =
    IO.ioRefEncoder ioRef


pointDecoder : Decode.Decoder Point
pointDecoder =
    Decode.map Pt IO.ioRefDecoder


type PointInfo
    = Info (IORef Int) (IORef Descriptor)
    | Link Point


pointInfoEncoder : PointInfo -> Encode.Value
pointInfoEncoder pointInfo =
    case pointInfo of
        Info weight desc ->
            Encode.object
                [ ( "type", Encode.string "Info" )
                , ( "weight", IO.ioRefEncoder weight )
                , ( "desc", IO.ioRefEncoder desc )
                ]

        Link point ->
            Encode.object
                [ ( "type", Encode.string "Link" )
                , ( "point", pointEncoder point )
                ]


pointInfoDecoder : Decode.Decoder PointInfo
pointInfoDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Info" ->
                        Decode.map2 Info
                            (Decode.field "weight" IO.ioRefDecoder)
                            (Decode.field "desc" IO.ioRefDecoder)

                    "Link" ->
                        Decode.map Link
                            (Decode.field "point" pointDecoder)

                    _ ->
                        Decode.fail ("Unknown PointInfo's type: " ++ type_)
            )



-- HELPERS


fresh : Descriptor -> IO Variable
fresh value =
    IO.newIORef Encode.int 1
        |> IO.bind
            (\weight ->
                IO.newIORef descriptorEncoder value
                    |> IO.bind (\desc -> IO.newIORef pointInfoEncoder (Info weight desc))
                    |> IO.fmap (\link -> Pt link)
            )


repr : Point -> IO Point
repr ((Pt ref) as point) =
    IO.readIORef pointInfoDecoder ref
        |> IO.bind
            (\pInfo ->
                case pInfo of
                    Info _ _ ->
                        IO.pure point

                    Link ((Pt ref1) as point1) ->
                        repr point1
                            |> IO.bind
                                (\point2 ->
                                    if point2 /= point1 then
                                        IO.readIORef pointInfoDecoder ref1
                                            |> IO.bind
                                                (\pInfo1 ->
                                                    IO.writeIORef pointInfoEncoder ref pInfo1
                                                        |> IO.fmap (\_ -> point2)
                                                )

                                    else
                                        IO.pure point2
                                )
            )


get : Point -> IO Descriptor
get ((Pt ref) as point) =
    IO.readIORef pointInfoDecoder ref
        |> IO.bind
            (\pInfo ->
                case pInfo of
                    Info _ descRef ->
                        IO.readIORef descriptorDecoder descRef

                    Link (Pt ref1) ->
                        IO.readIORef pointInfoDecoder ref1
                            |> IO.bind
                                (\link_ ->
                                    case link_ of
                                        Info _ descRef ->
                                            IO.readIORef descriptorDecoder descRef

                                        Link _ ->
                                            IO.bind get (repr point)
                                )
            )


set : Point -> Descriptor -> IO ()
set ((Pt ref) as point) newDesc =
    IO.readIORef pointInfoDecoder ref
        |> IO.bind
            (\pInfo ->
                case pInfo of
                    Info _ descRef ->
                        IO.writeIORef descriptorEncoder descRef newDesc

                    Link (Pt ref1) ->
                        IO.readIORef pointInfoDecoder ref1
                            |> IO.bind
                                (\link_ ->
                                    case link_ of
                                        Info _ descRef ->
                                            IO.writeIORef descriptorEncoder descRef newDesc

                                        Link _ ->
                                            repr point
                                                |> IO.bind
                                                    (\newPoint ->
                                                        set newPoint newDesc
                                                    )
                                )
            )


modify : Point -> (Descriptor -> Descriptor) -> IO ()
modify ((Pt ref) as point) func =
    IO.readIORef pointInfoDecoder ref
        |> IO.bind
            (\pInfo ->
                case pInfo of
                    Info _ descRef ->
                        IO.modifyIORef descriptorDecoder descriptorEncoder descRef func

                    Link (Pt ref1) ->
                        IO.readIORef pointInfoDecoder ref1
                            |> IO.bind
                                (\link_ ->
                                    case link_ of
                                        Info _ descRef ->
                                            IO.modifyIORef descriptorDecoder descriptorEncoder descRef func

                                        Link _ ->
                                            repr point
                                                |> IO.bind (\newPoint -> modify newPoint func)
                                )
            )


union : Point -> Point -> Descriptor -> IO ()
union p1 p2 newDesc =
    repr p1
        |> IO.bind
            (\((Pt ref1) as point1) ->
                repr p2
                    |> IO.bind
                        (\((Pt ref2) as point2) ->
                            IO.readIORef pointInfoDecoder ref1
                                |> IO.bind
                                    (\pointInfo1 ->
                                        IO.readIORef pointInfoDecoder ref2
                                            |> IO.bind
                                                (\pointInfo2 ->
                                                    case ( pointInfo1, pointInfo2 ) of
                                                        ( Info w1 d1, Info w2 d2 ) ->
                                                            if point1 == point2 then
                                                                IO.writeIORef descriptorEncoder d1 newDesc

                                                            else
                                                                IO.readIORef Decode.int w1
                                                                    |> IO.bind
                                                                        (\weight1 ->
                                                                            IO.readIORef Decode.int w2
                                                                                |> IO.bind
                                                                                    (\weight2 ->
                                                                                        let
                                                                                            newWeight =
                                                                                                weight1 + weight2
                                                                                        in
                                                                                        if weight1 >= weight2 then
                                                                                            IO.writeIORef pointInfoEncoder ref2 (Link point1)
                                                                                                |> IO.bind (\_ -> IO.writeIORef Encode.int w1 newWeight)
                                                                                                |> IO.bind (\_ -> IO.writeIORef descriptorEncoder d1 newDesc)

                                                                                        else
                                                                                            IO.writeIORef pointInfoEncoder ref1 (Link point2)
                                                                                                |> IO.bind (\_ -> IO.writeIORef Encode.int w2 newWeight)
                                                                                                |> IO.bind (\_ -> IO.writeIORef descriptorEncoder d2 newDesc)
                                                                                    )
                                                                        )

                                                        _ ->
                                                            crash "Unexpected pattern"
                                                )
                                    )
                        )
            )


equivalent : Point -> Point -> IO Bool
equivalent p1 p2 =
    repr p1
        |> IO.bind
            (\v1 ->
                repr p2
                    |> IO.fmap (\v2 -> v1 == v2)
            )


redundant : Point -> IO Bool
redundant (Pt ref) =
    IO.readIORef pointInfoDecoder ref
        |> IO.fmap
            (\pInfo ->
                case pInfo of
                    Info _ _ ->
                        False

                    Link _ ->
                        True
            )
