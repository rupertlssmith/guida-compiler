module Compiler.Json.String exposing
    ( fromChars
    , fromComment
    , fromName
    , fromSnippet
    , isEmpty
    , toChars
    )

import Compiler.Data.Name as Name
import Compiler.Parse.Primitives as P



-- JSON STRINGS


isEmpty : String -> Bool
isEmpty =
    String.isEmpty



-- FROM


fromChars : List Char -> String
fromChars =
    String.fromList


fromSnippet : P.Snippet -> String
fromSnippet (P.Snippet { fptr, offset, length }) =
    String.slice offset (offset + length) fptr


fromName : Name.Name -> String
fromName =
    identity



-- TO


toChars : String -> List Char
toChars =
    String.toList



-- FROM COMMENT


fromComment : P.Snippet -> String
fromComment (P.Snippet { fptr, offset, length }) =
    let
        pos =
            offset

        end =
            pos + length
    in
    fromChunks (chompChunks fptr pos end pos [])


chompChunks : String -> Int -> Int -> Int -> List Chunk -> List Chunk
chompChunks src pos end start revChunks =
    if pos >= end then
        List.reverse (addSlice start end revChunks)

    else
        let
            word =
                P.unsafeIndex src pos
        in
        case word of
            '\n' ->
                chompEscape src 'n' pos end start revChunks

            '"' ->
                chompEscape src '"' pos end start revChunks

            '\\' ->
                chompEscape src '\\' pos end start revChunks

            {- \r -}
            '\u{000D}' ->
                let
                    newPos =
                        pos + 1
                in
                chompChunks src newPos end newPos (addSlice start pos revChunks)

            _ ->
                let
                    width =
                        P.getCharWidth word

                    newPos =
                        pos + width
                in
                chompChunks src newPos end start revChunks


chompEscape : String -> Char -> Int -> Int -> Int -> List Chunk -> List Chunk
chompEscape src escape pos end start revChunks =
    let
        pos1 =
            pos + 1
    in
    chompChunks src pos1 end pos1 (Escape escape :: addSlice start pos revChunks)


addSlice : Int -> Int -> List Chunk -> List Chunk
addSlice start end revChunks =
    if start == end then
        revChunks

    else
        Slice start (end - start) :: revChunks



-- FROM CHUNKS


type Chunk
    = Slice Int Int
    | Escape Char


fromChunks : List Chunk -> String
fromChunks chunks =
    writeChunks 0 chunks


writeChunks : Int -> List Chunk -> String
writeChunks offset chunks =
    case chunks of
        [] ->
            ""

        chunk :: chunks_ ->
            case chunk of
                Slice _ len ->
                    let
                        newOffset =
                            offset + len
                    in
                    writeChunks newOffset chunks_

                Escape word ->
                    let
                        newOffset =
                            offset + 2
                    in
                    "\\" ++ String.fromChar word ++ writeChunks newOffset chunks_
