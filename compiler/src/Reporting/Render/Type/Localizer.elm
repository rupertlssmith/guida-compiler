module Reporting.Render.Type.Localizer exposing
    ( Localizer
    , empty
    , fromModule
    , fromNames
    , localizerDecoder
    , localizerEncoder
    , toChars
    , toDoc
    )

import AST.Source as Src
import AssocList as Dict exposing (Dict)
import Data.Name as Name exposing (Name)
import Elm.ModuleName as ModuleName
import EverySet exposing (EverySet)
import Json.Decode as Decode
import Json.DecodeX as DecodeX
import Json.Encode as Encode
import Json.EncodeX as EncodeX
import Reporting.Annotation as A
import Reporting.Doc as D



-- LOCALIZER


type Localizer
    = Localizer (Dict Name Import)


type alias Import =
    { alias : Maybe Name
    , exposing_ : Exposing
    }


type Exposing
    = All
    | Only (EverySet Name)


empty : Localizer
empty =
    Localizer Dict.empty



-- LOCALIZE


toDoc : Localizer -> ModuleName.Canonical -> Name -> D.Doc
toDoc localizer home name =
    D.fromChars (toChars localizer home name)


toChars : Localizer -> ModuleName.Canonical -> Name -> String
toChars (Localizer localizer) ((ModuleName.Canonical _ home) as moduleName) name =
    case Dict.get home localizer of
        Nothing ->
            home ++ "." ++ name

        Just import_ ->
            case import_.exposing_ of
                All ->
                    name

                Only set ->
                    if EverySet.member name set then
                        name

                    else if name == Name.list && moduleName == ModuleName.list then
                        "List"

                    else
                        Maybe.withDefault home import_.alias ++ "." ++ name



-- FROM NAMES


fromNames : Dict Name a -> Localizer
fromNames names =
    Localizer (Dict.map (\_ _ -> { alias = Nothing, exposing_ = All }) names)



-- FROM MODULE


fromModule : Src.Module -> Localizer
fromModule ((Src.Module _ _ _ imports _ _ _ _ _) as modul) =
    Localizer <|
        Dict.fromList <|
            (( Src.getName modul, { alias = Nothing, exposing_ = All } ) :: List.map toPair imports)


toPair : Src.Import -> ( Name, Import )
toPair (Src.Import (A.At _ name) alias_ exposing_) =
    ( name
    , Import alias_ (toExposing exposing_)
    )


toExposing : Src.Exposing -> Exposing
toExposing exposing_ =
    case exposing_ of
        Src.Open ->
            All

        Src.Explicit exposedList ->
            Only (List.foldr addType EverySet.empty exposedList)


addType : Src.Exposed -> EverySet Name -> EverySet Name
addType exposed types =
    case exposed of
        Src.Lower _ ->
            types

        Src.Upper (A.At _ name) _ ->
            EverySet.insert name types

        Src.Operator _ _ ->
            types



-- ENCODERS and DECODERS


localizerEncoder : Localizer -> Encode.Value
localizerEncoder (Localizer localizer) =
    EncodeX.assocListDict Encode.string importEncoder localizer


localizerDecoder : Decode.Decoder Localizer
localizerDecoder =
    Decode.map Localizer (DecodeX.assocListDict Decode.string importDecoder)


importEncoder : Import -> Encode.Value
importEncoder import_ =
    Encode.object
        [ ( "type", Encode.string "Import" )
        , ( "alias", EncodeX.maybe Encode.string import_.alias )
        , ( "exposing", exposingEncoder import_.exposing_ )
        ]


importDecoder : Decode.Decoder Import
importDecoder =
    Decode.map2 Import
        (Decode.field "alias" (Decode.maybe Decode.string))
        (Decode.field "exposing" exposingDecoder)


exposingEncoder : Exposing -> Encode.Value
exposingEncoder exposing_ =
    case exposing_ of
        All ->
            Encode.object
                [ ( "type", Encode.string "All" )
                ]

        Only set ->
            Encode.object
                [ ( "type", Encode.string "Only" )
                , ( "set", EncodeX.everySet Encode.string set )
                ]


exposingDecoder : Decode.Decoder Exposing
exposingDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "All" ->
                        Decode.succeed All

                    "Only" ->
                        Decode.map Only (Decode.field "set" (DecodeX.everySet Decode.string))

                    _ ->
                        Decode.fail ("Unknown Exposing's type: " ++ type_)
            )
