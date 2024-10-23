module Compiler.Elm.String exposing
    ( Chunk(..)
    , fromChunks
    )

import Hex



-- FROM CHUNKS


type Chunk
    = Slice Int Int
    | Escape Char
    | CodePoint Int


fromChunks : String -> List Chunk -> String
fromChunks src chunks =
    writeChunks src "" 0 chunks


writeChunks : String -> String -> Int -> List Chunk -> String
writeChunks src mba offset chunks =
    case chunks of
        [] ->
            mba

        chunk :: otherChunks ->
            case chunk of
                Slice ptr len ->
                    let
                        newOffset : Int
                        newOffset =
                            offset + len
                    in
                    writeChunks src (mba ++ String.slice ptr (ptr + len) src) newOffset otherChunks

                Escape word ->
                    let
                        newOffset : Int
                        newOffset =
                            offset + 2
                    in
                    writeChunks src (mba ++ "\\" ++ String.fromChar word) newOffset otherChunks

                CodePoint code ->
                    if code < 0xFFFF then
                        let
                            newOffset : Int
                            newOffset =
                                offset + 6
                        in
                        writeChunks src (mba ++ Hex.toString code) newOffset otherChunks

                    else
                        let
                            newOffset : Int
                            newOffset =
                                offset + 12
                        in
                        writeChunks src (mba ++ Hex.toString code) newOffset otherChunks
