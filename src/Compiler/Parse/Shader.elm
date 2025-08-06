module Compiler.Parse.Shader exposing (shader)

import Compiler.AST.Source as Src
import Compiler.AST.Utils.Shader as Shader
import Compiler.Parse.NewPrimitives as P
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E
import Data.Map as Dict
import Language.GLSL.Parser as GLP
import Language.GLSL.Syntax as GLS
import Parser exposing (..)
import Parser.Advanced as Advanced
import Utils.Crash as Crash



-- SHADER


shader : P.Parser E.Expr Src.Expr
shader =
    P.located
        (parseBlock
            |> andThen
                (\( block, startRow, startCol ) ->
                    parseGlsl startRow startCol block
                        |> map (\shdr -> Src.Shader (Shader.fromString block) shdr)
                )
        )



-- BLOCK


parseBlock : P.Parser E.Expr ( String, Int, Int )
parseBlock =
    Advanced.symbol "[glsl|" E.Start
        |> andThen
            (\_ ->
                P.getPosition
                    |> andThen
                        (\(A.Position row col) ->
                            getChompedString (chompUntil "|]")
                                |> andThen (\block -> succeed ( block, row, col ))
                        )
            )



-- GLSL


parseGlsl : Int -> Int -> String -> P.Parser E.Expr Shader.Types
parseGlsl startRow startCol src =
    case GLP.parse src of
        Ok (GLS.TranslationUnit decls) ->
            succeed (List.foldr addInput emptyTypes (List.concatMap extractInputs decls))

        Err { position, messages } ->
            -- FIXME this should be moved into guida-lang/glsl
            let
                lines : List String
                lines =
                    String.left position src
                        |> String.lines

                row : Int
                row =
                    List.length lines

                col : Int
                col =
                    case List.reverse lines of
                        lastLine :: _ ->
                            String.length lastLine

                        _ ->
                            0

                msg : String
                msg =
                    showErrorMessages messages
            in
            if row == 1 then
                failure startRow (startCol + 6 + col) msg

            else
                failure (startRow + row - 1) col msg


showErrorMessages : List String -> String
showErrorMessages msgs =
    if List.isEmpty msgs then
        "unknown parse error"

    else
        String.join "\n" msgs


failure : Int -> Int -> String -> P.Parser E.Expr a
failure row col msg =
    Advanced.problem (E.ShaderProblem msg)



-- INPUTS


emptyTypes : Shader.Types
emptyTypes =
    Shader.Types Dict.empty Dict.empty Dict.empty


addInput : ( GLS.StorageQualifier, Shader.Type, String ) -> Shader.Types -> Shader.Types
addInput ( qual, tipe, name ) (Shader.Types attribute uniform varying) =
    case qual of
        GLS.Attribute ->
            Shader.Types (Dict.insert name tipe attribute) uniform varying

        GLS.Uniform ->
            Shader.Types attribute (Dict.insert name tipe uniform) varying

        GLS.Varying ->
            Shader.Types attribute uniform (Dict.insert name tipe varying)

        _ ->
            Crash.crash "Should never happen due to `extractInputs` function"


extractInputs : GLS.ExternalDeclaration -> List ( GLS.StorageQualifier, Shader.Type, String )
extractInputs decl =
    case decl of
        GLS.Declaration (GLS.InitDeclaration (GLS.TypeDeclarator (GLS.FullType (Just (GLS.TypeQualSto qual)) (GLS.TypeSpec _ (GLS.TypeSpecNoPrecision tipe _)))) [ GLS.InitDecl name _ _ ]) ->
            if List.member qual [ GLS.Attribute, GLS.Varying, GLS.Uniform ] then
                case tipe of
                    GLS.Vec2 ->
                        [ ( qual, Shader.V2, name ) ]

                    GLS.Vec3 ->
                        [ ( qual, Shader.V3, name ) ]

                    GLS.Vec4 ->
                        [ ( qual, Shader.V4, name ) ]

                    GLS.Mat4 ->
                        [ ( qual, Shader.M4, name ) ]

                    GLS.Int ->
                        [ ( qual, Shader.Int, name ) ]

                    GLS.Float ->
                        [ ( qual, Shader.Float, name ) ]

                    GLS.Sampler2D ->
                        [ ( qual, Shader.Texture, name ) ]

                    _ ->
                        []

            else
                []

        _ ->
            []
