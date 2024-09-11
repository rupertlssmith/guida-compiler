module Json.EncodeX exposing
    ( Value(..)
    , array
    , assocListDict
    , bool
    , chars
    , dict
    , encode
    , encodeUgly
    , everySet
    , int
    , jsonPair
    , list
    , maybe
    , name
    , nonempty
    , null
    , number
    , object
    , oneOrMore
    , result
    , string
    , toJsonValue
    , write
    , writeUgly
    )

import Data.IO as IO exposing (IO(..))
import Data.Map as Dict exposing (Dict)
import Data.NonEmptyList as NE
import Data.OneOrMore as OneOrMore exposing (OneOrMore(..))
import Data.Set as EverySet exposing (EverySet)
import Json.Decode as Decode
import Json.Encode as Encode
import Json.StringX as Json



-- CORE HELPERS


assocListDict : (k -> Encode.Value) -> (v -> Encode.Value) -> Dict k v -> Encode.Value
assocListDict keyEncoder valueEncoder =
    Encode.list (jsonPair keyEncoder valueEncoder) << List.reverse << Dict.toList


jsonPair : (a -> Encode.Value) -> (b -> Encode.Value) -> ( a, b ) -> Encode.Value
jsonPair firstEncoder secondEncoder ( a, b ) =
    Encode.object
        [ ( "a", firstEncoder a )
        , ( "b", secondEncoder b )
        ]


everySet : (a -> Encode.Value) -> EverySet a -> Encode.Value
everySet encoder =
    Encode.list encoder << List.reverse << EverySet.toList


result : (x -> Encode.Value) -> (a -> Encode.Value) -> Result x a -> Encode.Value
result errEncoder successEncoder resultValue =
    case resultValue of
        Ok value ->
            Encode.object
                [ ( "type", Encode.string "Ok" )
                , ( "value", successEncoder value )
                ]

        Err err ->
            Encode.object
                [ ( "type", Encode.string "Err" )
                , ( "value", errEncoder err )
                ]


maybe : (a -> Encode.Value) -> Maybe a -> Encode.Value
maybe encoder maybeValue =
    case maybeValue of
        Just value ->
            encoder value

        Nothing ->
            Encode.null


nonempty : (a -> Encode.Value) -> NE.Nonempty a -> Encode.Value
nonempty encoder (NE.Nonempty x xs) =
    Encode.list encoder (x :: xs)


oneOrMore : (a -> Encode.Value) -> OneOrMore a -> Encode.Value
oneOrMore encoder oneOrMore_ =
    case oneOrMore_ of
        One value ->
            Encode.object [ ( "one", encoder value ) ]

        More left right ->
            Encode.object
                [ ( "left", oneOrMore encoder left )
                , ( "right", oneOrMore encoder right )
                ]



-- VALUES


type Value
    = Array (List Value)
    | Object (List ( String, Value ))
    | String String
    | Boolean Bool
    | Integer Int
    | Number Float
    | Null


array : List Value -> Value
array =
    Array


object : List ( String, Value ) -> Value
object =
    Object


string : String -> Value
string str =
    String ("\"" ++ str ++ "\"")


name : String -> Value
name nm =
    String ("\"" ++ nm ++ "\"")


bool : Bool -> Value
bool =
    Boolean


int : Int -> Value
int =
    Integer


number : Float -> Value
number =
    Number


null : Value
null =
    Null


dict : (k -> k -> Order) -> (k -> String) -> (v -> Value) -> Dict k v -> Value
dict keyComparison encodeKey encodeValue pairs =
    Object
        (Dict.toList pairs
            |> List.sortWith (\( ka, _ ) ( kb, _ ) -> keyComparison ka kb)
            |> List.map (\( k, v ) -> ( encodeKey k, encodeValue v ))
        )


list : (a -> Value) -> List a -> Value
list encodeEntry entries =
    Array (List.map encodeEntry entries)



-- CHARS


chars : List Char -> Value
chars chrs =
    String ("\"" ++ escape chrs ++ "\"")


escape : List Char -> String
escape chrs =
    case chrs of
        [] ->
            ""

        c :: cs ->
            let
                escapedChar =
                    case c of
                        '\u{000D}' ->
                            "\\r"

                        '\n' ->
                            "\\n"

                        '"' ->
                            "\\\""

                        '\\' ->
                            "\\\\"

                        _ ->
                            String.fromChar c
            in
            escapedChar ++ escape cs



-- WRITE TO FILE


write : String -> Value -> IO ()
write path value =
    fileWriteBuilder path (encode value ++ "\n")


writeUgly : String -> Value -> IO ()
writeUgly path value =
    fileWriteBuilder path (encodeUgly value)


fileWriteBuilder : String -> String -> IO ()
fileWriteBuilder path value =
    IO.make (Decode.succeed ()) (IO.WriteString path value)



-- ENCODE UGLY


encodeUgly : Value -> String
encodeUgly value =
    case value of
        Array [] ->
            "[]"

        Array entries ->
            "[" ++ String.join "," (List.map encodeUgly entries) ++ "]"

        Object [] ->
            "{}"

        Object entries ->
            "{" ++ String.join "," (List.map encodeEntryUgly entries) ++ "}"

        String builder ->
            builder

        Boolean boolean ->
            if boolean then
                "true"

            else
                "false"

        Integer n ->
            String.fromInt n

        Number scientific ->
            String.fromFloat scientific

        Null ->
            "null"


encodeEntryUgly : ( String, Value ) -> String
encodeEntryUgly ( key, entry ) =
    "\"" ++ key ++ "\":" ++ encodeUgly entry



-- ENCODE


encode : Value -> String
encode value =
    encodeHelp "" value


encodeHelp : String -> Value -> String
encodeHelp indent value =
    case value of
        Array [] ->
            "[]"

        Array (first :: rest) ->
            encodeArray indent first rest

        Object [] ->
            "{}"

        Object (first :: rest) ->
            encodeObject indent first rest

        String builder ->
            builder

        Boolean boolean ->
            if boolean then
                "true"

            else
                "false"

        Integer n ->
            String.fromInt n

        Number scientific ->
            String.fromFloat scientific

        Null ->
            "null"



-- ENCODE ARRAY


encodeArray : String -> Value -> List Value -> String
encodeArray indent first rest =
    let
        newIndent =
            indent ++ "    "

        closer =
            "\n" ++ indent ++ "]"

        addValue field builder =
            ",\n" ++ newIndent ++ encodeHelp newIndent field ++ builder
    in
    "[\n" ++ newIndent ++ encodeHelp newIndent first ++ List.foldr addValue closer rest



-- ENCODE OBJECT


encodeObject : String -> ( String, Value ) -> List ( String, Value ) -> String
encodeObject indent first rest =
    let
        newIndent =
            indent ++ "    "

        closer =
            "\n" ++ indent ++ "}"

        addValue field builder =
            ",\n" ++ newIndent ++ encodeField newIndent field ++ builder
    in
    "{\n" ++ newIndent ++ encodeField newIndent first ++ List.foldr addValue closer rest


encodeField : String -> ( String, Value ) -> String
encodeField indent ( key, value ) =
    "\"" ++ key ++ "\": " ++ encodeHelp indent value



-- JSON VALUE


toJsonValue : Value -> Encode.Value
toJsonValue value =
    case value of
        Array arr ->
            Encode.list toJsonValue arr

        Object obj ->
            Encode.object (List.map (Tuple.mapSecond toJsonValue) obj)

        String builder ->
            Encode.string builder

        Boolean boolean ->
            Encode.bool boolean

        Integer n ->
            Encode.int n

        Number scientific ->
            Encode.float scientific

        Null ->
            Encode.null
