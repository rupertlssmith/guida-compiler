module Compiler.Parse.Shader exposing (shader)

import Compiler.AST.Source as Src
import Compiler.AST.Utils.Shader as Shader
import Compiler.Parse.NewPrimitives as P
import Compiler.Reporting.Annotation as A
import Data.Map as Dict
import Language.GLSL.Parser as GLP
import Language.GLSL.Syntax as GLS
import Utils.Crash as Crash


shader : P.Parser Src.Expr
shader =
    P.addLocation (
        parseBlock
            |> P.andThen (\block ->
                parseGlsl block
                    |> P.map (\shdr -> Src.Shader (Shader.fromString block) shdr)
            )
    )


parseBlock : P.Parser String
parseBlock =
    P.succeed ()
        |. P.token "[glsl|" (P.Problem_Expr P.EP_Start)
        |> P.andThen (\_ ->
            P.getChompedString (P.chompUntil (P.Token "|]" (P.Problem_Expr P.EP_EndlessShader)))
           )


parseGlsl : String -> P.Parser Shader.Types
parseGlsl src =
    case GLP.parse src of
        Ok (GLS.TranslationUnit decls) ->
            P.succeed (List.foldr addInput emptyTypes (List.concatMap extractInputs decls))

        Err { position, messages } ->
            let
                msg : String
                msg =
                    showErrorMessages messages
            in
            P.problem (P.Problem_Expr (P.EP_ShaderProblem msg))


showErrorMessages : List String -> String
showErrorMessages msgs =
    if List.isEmpty msgs then
        "unknown parse error"

    else
        String.join "\n" msgs



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
