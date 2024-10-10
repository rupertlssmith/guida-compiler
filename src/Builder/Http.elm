module Builder.Http exposing
    ( Error(..)
    , Header
    , HttpExceptionContent(..)
    , Manager
    , Sha
    , accept
    , errorDecoder
    , errorEncoder
    , filePart
    , get
    , getArchive
    , getManager
    , jsonPart
    , managerDecoder
    , managerEncoder
    , post
    , shaToChars
    , stringPart
    , toUrl
    , upload
    )

import Basics.Extra exposing (uncurry)
import Compiler.Elm.Version as V
import Compiler.Parse.Keyword exposing (type_)
import Data.IO as IO exposing (IO(..))
import Json.Decode as Decode
import Json.Encode as Encode
import Url.Builder
import Utils.Crash exposing (todo)
import Utils.Main as Utils exposing (HTTPResponse(..), SomeException(..))



-- MANAGER


type Manager
    = Manager


managerEncoder : Manager -> Encode.Value
managerEncoder _ =
    Encode.object [ ( "type", Encode.string "Manager" ) ]


managerDecoder : Decode.Decoder Manager
managerDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Manager" ->
                        Decode.succeed Manager

                    _ ->
                        Decode.fail "Failed to decode Http.Manager"
            )


getManager : IO Manager
getManager =
    -- TODO newManager tlsManagerSettings
    IO.pure Manager



-- URL


toUrl : String -> List ( String, String ) -> String
toUrl url params =
    case params of
        [] ->
            url

        _ :: _ ->
            url ++ urlEncodeVars params


urlEncodeVars : List ( String, String ) -> String
urlEncodeVars params =
    -- includes the `?`
    Url.Builder.toQuery (List.map (uncurry Url.Builder.string) params)



-- FETCH


type Header
    = Header


get : Manager -> String -> List Header -> (Error -> e) -> (String -> IO (Result e a)) -> IO (Result e a)
get =
    fetch MethodGet


post : Manager -> String -> List Header -> (Error -> e) -> (String -> IO (Result e a)) -> IO (Result e a)
post =
    fetch MethodPost


type Method
    = MethodGet
    | MethodPost


fetch : Method -> Manager -> String -> List Header -> (Error -> e) -> (String -> IO (Result e a)) -> IO (Result e a)
fetch methodVerb manager url headers onError onSuccess =
    -- handle (handleSomeException url onError) <|
    --     handle (handleHttpException url onError) <|
    --         (parseUrlThrow url
    --             |> IO.bind
    --                 (\req0 ->
    --                     let
    --                         req1 =
    --                             req0
    --                                 { method = methodVerb
    --                                 , requestHeaders = addDefaultHeaders headers
    --                                 }
    --                     in
    --                     withResponse req1
    --                         manager
    --                         (\response ->
    --                             brConsume (responseBody response)
    --                                 |> IO.bind
    --                                     (\chunks ->
    --                                         onSuccess (BS.concat chunks)
    --                                     )
    --                         )
    --                 )
    --         )
    -- IO.pure (Err (onError (BadHttp url (ConnectionFailure SomeException))))
    IO.make Decode.string
        (IO.HttpFetch
            (case methodVerb of
                MethodGet ->
                    "GET"

                MethodPost ->
                    "POST"
            )
            url
        )
        |> IO.bind onSuccess


addDefaultHeaders : List Header -> List Header
addDefaultHeaders headers =
    -- ( hUserAgent, userAgent ) :: ( hAcceptEncoding, "gzip" ) :: headers
    todo "addDefaultHeaders"


userAgent : String
userAgent =
    "elm/" ++ V.toChars V.compiler


accept : String -> Header
accept mime =
    -- ( hAccept, mime )
    todo "accept"



-- EXCEPTIONS


type Error
    = BadUrl String String
    | BadHttp String HttpExceptionContent
    | BadMystery String SomeException


type HttpExceptionContent
    = StatusCodeException (HTTPResponse ()) String
    | TooManyRedirects (List (HTTPResponse ()))
    | ConnectionFailure SomeException


type HttpException
    = HttpException


handleHttpException : String -> (Error -> e) -> HttpException -> IO (Result e a)
handleHttpException url onError httpException =
    -- case httpException of
    --     InvalidUrlException _ reason ->
    --         IO.pure (Err (onError (BadUrl url reason)))
    --     HttpExceptionRequest _ content ->
    --         IO.pure (Err (onError (BadHttp url content)))
    todo "handleHttpException"


handleSomeException : String -> (Error -> e) -> SomeException -> IO (Result e a)
handleSomeException url onError exception =
    IO.pure (Err (onError (BadMystery url exception)))



-- SHA


type alias Sha =
    String


shaToChars : Sha -> String
shaToChars =
    identity



-- FETCH ARCHIVE


getArchive : Manager -> String -> (Error -> e) -> e -> (( Sha, Utils.ZipArchive ) -> IO (Result e a)) -> IO (Result e a)
getArchive manager url onError err onSuccess =
    IO.make Utils.zipArchiveDecoder (IO.GetArchive "GET" url)
        |> IO.bind
            (\archive ->
                -- TODO review the need to use `readArchive...`
                onSuccess ( "SHA-TODO", archive )
            )



-- UPLOAD


type MultiPart
    = MultiPart


upload : Manager -> String -> List MultiPart -> IO (Result Error ())
upload manager url parts =
    -- handle (handleSomeException url id) <|
    --     handle (handleHttpException url id) <|
    --         do req0
    --             <- parseUrlThrow url
    --                 req1
    --             <- Multi.formDataBody parts
    --         <|
    --             req0
    --                 { method = methodPost
    --                 , requestHeaders = addDefaultHeaders []
    --                 , responseTimeout = responseTimeoutNone
    --                 }
    --                 withResponse
    --                 req1
    --                 manager
    --             <|
    --                 \_ ->
    --                     return (Right ())
    todo "upload"


filePart : String -> String -> MultiPart
filePart name filePath =
    -- Multi.partFileSource (String.fromString name) filePath
    todo "filePart"


jsonPart : String -> String -> Encode.Value -> MultiPart
jsonPart name filePath value =
    -- let
    --     body =
    --         Multi.RequestBodyLBS <| B.toLazyByteString <| Encode.encodeUgly value
    -- in
    -- Multi.partFileRequestBody (String.fromString name) filePath body
    todo "jsonPart"


stringPart : String -> String -> MultiPart
stringPart name string =
    -- Multi.partBS (String.fromString name) (BS.pack string)
    todo "stringPart"



-- ENCODERS and DECODERS


errorEncoder : Error -> Encode.Value
errorEncoder error =
    case error of
        BadUrl url reason ->
            Encode.object
                [ ( "type", Encode.string "BadUrl" )
                , ( "url", Encode.string url )
                , ( "reason", Encode.string reason )
                ]

        BadHttp url httpExceptionContent ->
            Encode.object
                [ ( "type", Encode.string "BadHttp" )
                , ( "url", Encode.string url )
                , ( "httpExceptionContent", httpExceptionContentEncoder httpExceptionContent )
                ]

        BadMystery url someException ->
            Encode.object
                [ ( "type", Encode.string "BadMystery" )
                , ( "url", Encode.string url )
                , ( "someException", Utils.someExceptionEncoder someException )
                ]


errorDecoder : Decode.Decoder Error
errorDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "BadUrl" ->
                        Decode.map2 BadUrl
                            (Decode.field "url" Decode.string)
                            (Decode.field "reason" Decode.string)

                    "BadHttp" ->
                        Decode.map2 BadHttp
                            (Decode.field "url" Decode.string)
                            (Decode.field "httpExceptionContent" httpExceptionContentDecoder)

                    "BadMystery" ->
                        Decode.map2 BadMystery
                            (Decode.field "url" Decode.string)
                            (Decode.field "someException" Utils.someExceptionDecoder)

                    _ ->
                        Decode.fail ("Failed to decode Error's type: " ++ type_)
            )


httpExceptionContentEncoder : HttpExceptionContent -> Encode.Value
httpExceptionContentEncoder httpExceptionContent =
    case httpExceptionContent of
        StatusCodeException response body ->
            Encode.object
                [ ( "type", Encode.string "StatusCodeException" )
                , ( "response", Utils.httpResponseEncoder response )
                , ( "body", Encode.string body )
                ]

        TooManyRedirects responses ->
            Encode.object
                [ ( "type", Encode.string "TooManyRedirects" )
                , ( "responses", Encode.list Utils.httpResponseEncoder responses )
                ]

        ConnectionFailure someException ->
            Encode.object
                [ ( "type", Encode.string "ConnectionFailure" )
                , ( "someException", Utils.someExceptionEncoder someException )
                ]


httpExceptionContentDecoder : Decode.Decoder HttpExceptionContent
httpExceptionContentDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "StatusCodeException" ->
                        Decode.map2 StatusCodeException
                            (Decode.field "response" Utils.httpResponseDecoder)
                            (Decode.field "body" Decode.string)

                    "TooManyRedirects" ->
                        Decode.map TooManyRedirects (Decode.field "responses" (Decode.list Utils.httpResponseDecoder))

                    "ConnectionFailure" ->
                        Decode.map ConnectionFailure (Decode.field "someException" Utils.someExceptionDecoder)

                    _ ->
                        Decode.fail ("Failed to decode HttpExceptionContent's type: " ++ type_)
            )
