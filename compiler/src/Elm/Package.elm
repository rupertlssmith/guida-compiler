module Elm.Package exposing
    ( Author
    , Canonical(..)
    , Name(..)
    , Project
    , browser
    , compareName
    , core
    , decoder
    , dummyName
    , encode
    , html
    , http
    , isKernel
    , json
    , kernel
    , keyDecoder
    , linearAlgebra
    , nameDecoder
    , nameEncoder
    , nearbyNames
    , parser
    , suggestions
    , toChars
    , toFilePath
    , toJsonString
    , toString
    , toUrl
    , url
    , virtualDom
    , webgl
    )

import AssocList as Dict exposing (Dict)
import Elm.Version as V
import Json.Decode as Decode
import Json.DecodeX as D
import Json.Encode as Encode
import Json.EncodeX as E
import Parse.Primitives as P exposing (Col, Row)
import Reporting.Suggest as Suggest



-- PACKAGE NAMES


type Name
    = Name Author Project


toString : Name -> String
toString (Name author project) =
    author ++ "/" ++ project


compareName : Name -> Name -> Order
compareName (Name name1 project1) (Name name2 project2) =
    case compare name1 name2 of
        LT ->
            LT

        EQ ->
            compare project1 project2

        GT ->
            GT


type alias Author =
    String


type alias Project =
    String


type Canonical
    = Canonical Name V.Version



-- HELPERS


isKernel : Name -> Bool
isKernel (Name author _) =
    author == elm || author == elm_explorations


toChars : Name -> String
toChars (Name author project) =
    author ++ "/" ++ project


toUrl : Name -> String
toUrl (Name author project) =
    author ++ "/" ++ project


toFilePath : Name -> String
toFilePath (Name author project) =
    author ++ "/" ++ project


toJsonString : Name -> String
toJsonString (Name author project) =
    String.join "/" [ author, project ]



-- COMMON PACKAGE NAMES


toName : Author -> Project -> Name
toName =
    Name


dummyName : Name
dummyName =
    toName "author" "project"


kernel : Name
kernel =
    toName elm "kernel"


core : Name
core =
    toName elm "core"


browser : Name
browser =
    toName elm "browser"


virtualDom : Name
virtualDom =
    toName elm "virtual-dom"


html : Name
html =
    toName elm "html"


json : Name
json =
    toName elm "json"


http : Name
http =
    toName elm "http"


url : Name
url =
    toName elm "url"


webgl : Name
webgl =
    toName elm_explorations "webgl"


linearAlgebra : Name
linearAlgebra =
    toName elm_explorations "linear-algebra"


elm : Author
elm =
    "elm"


elm_explorations : Author
elm_explorations =
    "elm-explorations"



-- PACKAGE SUGGESTIONS


suggestions : Dict String Name
suggestions =
    let
        random =
            toName elm "random"

        time =
            toName elm "time"

        file =
            toName elm "file"
    in
    Dict.fromList
        [ ( "Browser", browser )
        , ( "File", file )
        , ( "File.Download", file )
        , ( "File.Select", file )
        , ( "Html", html )
        , ( "Html.Attributes", html )
        , ( "Html.Events", html )
        , ( "Http", http )
        , ( "Json.Decode", json )
        , ( "Json.Encode", json )
        , ( "Random", random )
        , ( "Time", time )
        , ( "Url.Parser", url )
        , ( "Url", url )
        ]



-- NEARBY NAMES


nearbyNames : Name -> List Name -> List Name
nearbyNames (Name author1 project1) possibleNames =
    let
        authorDist =
            authorDistance author1

        projectDist =
            projectDistance project1

        nameDistance (Name author2 project2) =
            authorDist author2 + projectDist project2
    in
    List.take 4 (List.sortBy nameDistance possibleNames)


authorDistance : String -> Author -> Int
authorDistance given possibility =
    if possibility == elm || possibility == elm_explorations then
        0

    else
        abs (Suggest.distance given possibility)


projectDistance : String -> Project -> Int
projectDistance given possibility =
    abs (Suggest.distance given possibility)



-- JSON


decoder : D.Decoder ( Row, Col ) Name
decoder =
    D.customString parser Tuple.pair


encode : Name -> E.Value
encode name =
    E.string (toChars name)


keyDecoder : (Row -> Col -> x) -> D.KeyDecoder x Name
keyDecoder toError =
    let
        keyParser =
            P.specialize (\( r, c ) _ _ -> toError r c) parser
    in
    D.KeyDecoder keyParser toError



-- PARSER


parser : P.Parser ( Row, Col ) Name
parser =
    parseName isAlphaOrDigit isAlphaOrDigit
        |> P.bind
            (\author ->
                P.word1 '/' Tuple.pair
                    |> P.bind (\_ -> parseName isLower isLowerOrDigit)
                    |> P.fmap
                        (\project -> Name author project)
            )


parseName : (Char -> Bool) -> (Char -> Bool) -> P.Parser ( Row, Col ) String
parseName isGoodStart isGoodInner =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            if pos >= end then
                Err (P.PErr P.Empty row col Tuple.pair)

            else
                let
                    word =
                        P.unsafeIndex src pos
                in
                if not (isGoodStart word) then
                    Err (P.PErr P.Empty row col Tuple.pair)

                else
                    let
                        ( isGood, newPos ) =
                            chompName isGoodInner src (pos + 1) end False

                        len =
                            newPos - pos

                        newCol =
                            col + len
                    in
                    if isGood && len < 256 then
                        let
                            newState =
                                P.State src newPos end indent row newCol
                        in
                        Ok (P.POk P.Consumed (String.slice pos newPos src) newState)

                    else
                        Err (P.PErr P.Consumed row newCol Tuple.pair)


isLower : Char -> Bool
isLower =
    Char.isLower


isLowerOrDigit : Char -> Bool
isLowerOrDigit word =
    Char.isLower word || Char.isDigit word


isAlphaOrDigit : Char -> Bool
isAlphaOrDigit =
    Char.isAlphaNum


chompName : (Char -> Bool) -> String -> Int -> Int -> Bool -> ( Bool, Int )
chompName isGoodChar src pos end prevWasDash =
    if pos >= end then
        ( not prevWasDash, pos )

    else
        let
            word =
                P.unsafeIndex src pos
        in
        if isGoodChar word then
            chompName isGoodChar src (pos + 1) end False

        else if word == '-' then
            if prevWasDash then
                ( False, pos )

            else
                chompName isGoodChar src (pos + 1) end True

        else
            ( True, pos )



-- ENCODERS and DECODERS


nameEncoder : Name -> Encode.Value
nameEncoder (Name author project) =
    Encode.object
        [ ( "author", Encode.string author )
        , ( "project", Encode.string project )
        ]


nameDecoder : Decode.Decoder Name
nameDecoder =
    Decode.map2 Name
        (Decode.field "author" Decode.string)
        (Decode.field "project" Decode.string)
