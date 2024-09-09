module Parse.Shader exposing (shader)

import AST.Source as Src
import AST.Utils.Shader as Shader
import AssocList as Dict
import Data.Name as Name
import Parse.Primitives as P exposing (Col, Parser, Row)
import Reporting.Annotation as A
import Reporting.Error.Syntax as E



-- SHADER


shader : A.Position -> Parser E.Expr Src.Expr
shader ((A.Position row col) as start) =
    parseBlock
        |> P.bind
            (\block ->
                parseGlsl row col block
                    |> P.bind
                        (\shdr ->
                            P.getPosition
                                |> P.fmap
                                    (\end ->
                                        A.at start end (Src.Shader (Shader.fromString block) shdr)
                                    )
                        )
            )



-- BLOCK


parseBlock : Parser E.Expr String
parseBlock =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            let
                pos6 =
                    pos + 6
            in
            if
                (pos6 <= end)
                    && (P.unsafeIndex src pos == '[')
                    && (P.unsafeIndex src (pos + 1) == 'g')
                    && (P.unsafeIndex src (pos + 2) == 'l')
                    && (P.unsafeIndex src (pos + 3) == 's')
                    && (P.unsafeIndex src (pos + 4) == 'l')
                    && (P.unsafeIndex src (pos + 5) == '|')
            then
                let
                    ( ( status, newPos ), ( newRow, newCol ) ) =
                        eatShader src pos6 end row (col + 6)
                in
                case status of
                    Good ->
                        let
                            off =
                                pos6

                            len =
                                newPos - pos6

                            block =
                                String.slice off len src

                            newState =
                                P.State src (newPos + 2) end indent newRow (newCol + 2)
                        in
                        Ok (P.POk P.Consumed block newState)

                    Unending ->
                        Err (P.PErr P.Consumed row col E.EndlessShader)

            else
                Err (P.PErr P.Empty row col E.Start)


type Status
    = Good
    | Unending


eatShader : String -> Int -> Int -> Row -> Col -> ( ( Status, Int ), ( Row, Col ) )
eatShader src pos end row col =
    if pos >= end then
        ( ( Unending, pos ), ( row, col ) )

    else
        let
            word =
                P.unsafeIndex src pos
        in
        if word == '|' && P.isWord src (pos + 1) end ']' then
            ( ( Good, pos ), ( row, col ) )

        else if word == '\n' then
            eatShader src (pos + 1) end (row + 1) 1

        else
            let
                newPos =
                    pos + P.getCharWidth word
            in
            eatShader src newPos end row (col + 1)



-- GLSL


parseGlsl : Row -> Col -> String -> Parser E.Expr Shader.Types
parseGlsl startRow startCol src =
    -- case GLP.parse src of
    --     Ok (GLS.TranslationUnit decls) ->
    --         P.succeed (List.foldr addInput emptyTypes (List.concatMap extractInputs decls))
    --     Err err ->
    --         let
    --             pos =
    --                 Parsec.errorPos err
    --             row =
    --                 fromIntegral (Parsec.sourceLine pos)
    --             col =
    --                 fromIntegral (Parsec.sourceColumn pos)
    --             msg =
    --                 Parsec.showErrorMessages
    --                     "or"
    --                     "unknown parse error"
    --                     "expecting"
    --                     "unexpected"
    --                     "end of input"
    --                     (Parsec.errorMessages err)
    --         in
    --         if row == 1 then
    --             failure startRow (startCol + 6 + col) msg
    --         else
    --             failure (startRow + row - 1) col msg
    Debug.todo "parseGlsl"


failure : Row -> Col -> String -> Parser E.Expr a
failure row col msg =
    P.Parser <|
        \_ ->
            Err (P.PErr P.Consumed row col (E.ShaderProblem msg))



-- INPUTS


emptyTypes : Shader.Types
emptyTypes =
    Shader.Types Dict.empty Dict.empty Dict.empty



-- addInput : ( GLS.StorageQualifier, Shader.Type, String ) -> Shader.Types -> Shader.Types
-- addInput ( qual, tipe, name ) glDecls =
--     case qual of
--         GLS.Attribute ->
--             { glDecls | attribute = Dict.insert (Name.fromChars name) tipe glDecls.attribute }
--         GLS.Uniform ->
--             { glDecls | uniform = Dict.insert (Name.fromChars name) tipe glDecls.uniform }
--         GLS.Varying ->
--             { glDecls | varying = Dict.insert (Name.fromChars name) tipe glDecls.varying }
--         _ ->
--             Debug.crash "Should never happen due to `extractInputs` function"
-- extractInputs : GLS.ExternalDeclaration -> List ( GLS.StorageQualifier, Shader.Type, String )
-- extractInputs decl =
--     case decl of
--         GLS.Declaration (GLS.InitDeclaration (GLS.TypeDeclarator (GLS.FullType (Just (GLS.TypeQualSto qual)) (GLS.TypeSpec _ prec (GLS.TypeSpecNoPrecision tipe _ mexpr1)))) [ GLS.InitDecl name _ mexpr2 _ mexpr3 ]) ->
--             if List.member qual [ GLS.Attribute, GLS.Varying, GLS.Uniform ] then
--                 case tipe of
--                     GLS.Vec2 ->
--                         [ ( qual, Shader.V2, name ) ]
--                     GLS.Vec3 ->
--                         [ ( qual, Shader.V3, name ) ]
--                     GLS.Vec4 ->
--                         [ ( qual, Shader.V4, name ) ]
--                     GLS.Mat4 ->
--                         [ ( qual, Shader.M4, name ) ]
--                     GLS.Int ->
--                         [ ( qual, Shader.Int, name ) ]
--                     GLS.Float ->
--                         [ ( qual, Shader.Float, name ) ]
--                     GLS.Sampler2D ->
--                         [ ( qual, Shader.Texture, name ) ]
--                     _ ->
--                         []
--             else
--                 []
--         _ ->
--             []
