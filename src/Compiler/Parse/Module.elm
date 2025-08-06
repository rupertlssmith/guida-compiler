module Compiler.Parse.Module exposing
    ( ProjectType(..)
    , chompImport
    , chompImports
    , fromByteString
    , isKernel
    )

import Compiler.AST.Source as Src
import Compiler.Data.Name as Name
import Compiler.Elm.Compiler.Imports as Imports
import Compiler.Elm.Package as Pkg
import Compiler.Parse.Declaration as Decl
import Compiler.Parse.Keyword as Keyword
import Compiler.Parse.NewPrimitives as P
import Compiler.Parse.Space as Space
import Compiler.Parse.Symbol as Symbol
import Compiler.Parse.SyntaxVersion exposing (SyntaxVersion)
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E
import Parser exposing (..)
import Parser.Advanced as Advanced



-- FROM BYTE STRING


fromByteString : SyntaxVersion -> ProjectType -> String -> Result E.Error Src.Module
fromByteString syntaxVersion projectType source =
    case Parser.run (chompModule syntaxVersion projectType) source of
        Ok modul ->
            checkModule syntaxVersion projectType modul

        Err err ->
            Err (E.ParseError err)



-- PROJECT TYPE


type ProjectType
    = Package Pkg.Name
    | Application


isCore : ProjectType -> Bool
isCore projectType =
    case projectType of
        Package pkg ->
            pkg == Pkg.core

        Application ->
            False


isKernel : ProjectType -> Bool
isKernel projectType =
    case projectType of
        Package pkg ->
            Pkg.isKernel pkg

        Application ->
            False



-- MODULE


type alias Module =
    { header : Maybe Header
    , imports : List Src.Import
    , infixes : List (A.Located Src.Infix)
    , decls : List Decl.Decl
    }


chompModule : SyntaxVersion -> ProjectType -> P.Parser E.Module Module
chompModule syntaxVersion projectType =
    succeed Module
        |> andMap chompHeader
        |> andMap
            (chompImports
                (if isCore projectType then
                    []

                 else
                    Imports.defaults
                )
            )
        |> andMap
            (if isKernel projectType then
                chompInfixes []

             else
                succeed []
            )
        |> andMap (loop [] (chompDeclsHelp syntaxVersion))



-- CHECK MODULE


checkModule : SyntaxVersion -> ProjectType -> Module -> Result E.Error Src.Module
checkModule syntaxVersion projectType module_ =
    let
        ( ( values, unions ), ( aliases, ports ) ) =
            categorizeDecls [] [] [] [] module_.decls
    in
    case module_.header of
        Just { name, effects, exports, docs } ->
            checkEffects projectType ports effects
                |> Result.map
                    (Src.Module syntaxVersion
                        (Just name)
                        exports
                        (toDocs docs module_.decls)
                        module_.imports
                        values
                        unions
                        aliases
                        module_.infixes
                    )

        Nothing ->
            Ok
                (Src.Module syntaxVersion
                    Nothing
                    (A.At A.one Src.Open)
                    (Src.NoDocs A.one)
                    module_.imports
                    values
                    unions
                    aliases
                    module_.infixes
                    (case ports of
                        [] ->
                            Src.NoEffects

                        _ ->
                            Src.Ports ports
                    )
                )


checkEffects : ProjectType -> List Src.Port -> Effects -> Result E.Error Src.Effects
checkEffects projectType ports effects =
    case effects of
        NoEffects region ->
            case ports of
                [] ->
                    Ok Src.NoEffects

                (Src.Port name _) :: _ ->
                    case projectType of
                        Package _ ->
                            Err (E.NoPortsInPackage name)

                        Application ->
                            Err (E.UnexpectedPort region)

        Ports region ->
            case projectType of
                Package _ ->
                    Err (E.NoPortModulesInPackage region)

                Application ->
                    case ports of
                        [] ->
                            Err (E.NoPorts region)

                        _ :: _ ->
                            Ok (Src.Ports ports)

        Manager region manager ->
            if isKernel projectType then
                case ports of
                    [] ->
                        Ok (Src.Manager region manager)

                    _ :: _ ->
                        Err (E.UnexpectedPort region)

            else
                Err (E.NoEffectsOutsideKernel region)


categorizeDecls : List (A.Located Src.Value) -> List (A.Located Src.Union) -> List (A.Located Src.Alias) -> List Src.Port -> List Decl.Decl -> ( ( List (A.Located Src.Value), List (A.Located Src.Union) ), ( List (A.Located Src.Alias), List Src.Port ) )
categorizeDecls values unions aliases ports decls =
    case decls of
        [] ->
            ( ( values, unions ), ( aliases, ports ) )

        decl :: otherDecls ->
            case decl of
                Decl.Value _ value ->
                    categorizeDecls (value :: values) unions aliases ports otherDecls

                Decl.Union _ union ->
                    categorizeDecls values (union :: unions) aliases ports otherDecls

                Decl.Alias _ alias_ ->
                    categorizeDecls values unions (alias_ :: aliases) ports otherDecls

                Decl.Port _ port_ ->
                    categorizeDecls values unions aliases (port_ :: ports) otherDecls



-- TO DOCS


toDocs : Result A.Region Src.Comment -> List Decl.Decl -> Src.Docs
toDocs comment decls =
    case comment of
        Ok overview ->
            Src.YesDocs overview (getComments decls [])

        Err region ->
            Src.NoDocs region


getComments : List Decl.Decl -> List ( Name.Name, Src.Comment ) -> List ( Name.Name, Src.Comment )
getComments decls comments =
    case decls of
        [] ->
            comments

        decl :: otherDecls ->
            case decl of
                Decl.Value c (A.At _ (Src.Value n _ _ _)) ->
                    getComments otherDecls (addComment c n comments)

                Decl.Union c (A.At _ (Src.Union n _ _)) ->
                    getComments otherDecls (addComment c n comments)

                Decl.Alias c (A.At _ (Src.Alias n _ _)) ->
                    getComments otherDecls (addComment c n comments)

                Decl.Port c (Src.Port n _) ->
                    getComments otherDecls (addComment c n comments)


addComment : Maybe Src.Comment -> A.Located Name.Name -> List ( Name.Name, Src.Comment ) -> List ( Name.Name, Src.Comment )
addComment maybeComment (A.At _ name) comments =
    case maybeComment of
        Just comment ->
            ( name, comment ) :: comments

        Nothing ->
            comments



-- FRESH LINES


freshLine : (P.Row -> P.Col -> E.Module) -> P.Parser E.Module ()
freshLine toFreshLineError =
    Space.chomp E.ModuleSpace
        |> andThen (\_ -> Space.checkFreshLine toFreshLineError)



-- CHOMP DECLARATIONS


chompDeclsHelp : SyntaxVersion -> List Decl.Decl -> P.Parser E.Decl (Step (List Decl.Decl) (List Decl.Decl))
chompDeclsHelp syntaxVersion revDecls =
    oneOf
        [ Decl.declaration syntaxVersion |> andThen (\( d, _ ) -> succeed (Loop (d :: revDecls)))
        , succeed (Done (List.reverse revDecls))
        ]


chompInfixes : List (A.Located Src.Infix) -> P.Parser E.Module (List (A.Located Src.Infix))
chompInfixes infixes =
    loop infixes chompInfixesHelp


chompInfixesHelp : List (A.Located Src.Infix) -> P.Parser E.Module (Step (List (A.Located Src.Infix)) (List (A.Located Src.Infix)))
chompInfixesHelp revInfixes =
    oneOf
        [ Decl.infix_ |> andThen (\i -> succeed (Loop (i :: revInfixes)))
        , succeed (Done (List.reverse revInfixes))
        ]



-- MODULE DOC COMMENT


chompModuleDocCommentSpace : P.Parser E.Module (Result A.Region Src.Comment)
chompModuleDocCommentSpace =
    P.located (freshLine E.FreshLine)
        |> andThen
            (\(A.At region ()) ->
                oneOf
                    [ Space.docComment E.ImportStart E.ModuleSpace
                        |> andThen
                            (\docComment ->
                                Space.chomp E.ModuleSpace
                                    |> andThen (\_ -> Space.checkFreshLine E.FreshLine)
                                    |> map (\_ -> Ok docComment)
                            )
                    ]
                    |> oneOfWithFallback (Err region)
            )



-- HEADER


type alias Header =
    { name : A.Located Name.Name
    , effects : Effects
    , exports : A.Located Src.Exposing
    , docs : Result A.Region Src.Comment
    }


type Effects
    = NoEffects A.Region
    | Ports A.Region
    | Manager A.Region Src.Manager


chompHeader : P.Parser E.Module (Maybe Header)
chompHeader =
    freshLine E.FreshLine
        |> andThen (\_ -> P.getPosition)
        |> andThen
            (\start ->
                oneOf
                    [ Keyword.module_ E.ModuleProblem
                        |> andThen (\_ -> chompHeader_ start (NoEffects (A.Region start start)))
                    , Keyword.port_ E.PortModuleProblem
                        |> andThen
                            (\_ ->
                                Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem
                                    |> andThen (\_ -> Keyword.module_ E.PortModuleProblem)
                                    |> andThen (\_ -> chompHeader_ start (Ports (A.Region start start)))
                            )
                    , Keyword.effect_ E.Effect
                        |> andThen
                            (\_ ->
                                Space.chompAndCheckIndent E.ModuleSpace E.Effect
                                    |> andThen (\_ -> Keyword.module_ E.Effect)
                                    |> andThen (\_ -> chompHeader_ start (Manager (A.Region start start) Src.Cmd))
                            )
                    ]
                    |> oneOfWithFallback Nothing
            )


chompHeader_ : A.Position -> Effects -> P.Parser E.Module (Maybe Header)
chompHeader_ start effects =
    succeed (\n e d -> Just (Header n effects e d))
        |> andMap (Space.chompAndCheckIndent E.ModuleSpace E.ModuleProblem |> andThen (\_ -> P.located (Var.moduleName E.ModuleName)))
        |> andMap (Space.chompAndCheckIndent E.ModuleSpace E.ModuleProblem |> andThen (\_ -> Keyword.exposing_ E.ModuleProblem) |> andThen (\_ -> P.located exposing_))
        |> andMap chompModuleDocCommentSpace


chompManager : P.Parser E.Module Src.Manager
chompManager =
    Advanced.symbol "{" E.Effect
        |> andThen
            (\_ ->
                oneOf
                    [ chompCommand |> andThen (\c -> succeed (Src.Cmd c))
                    , chompSubscription |> andThen (\s -> succeed (Src.Sub s))
                    , succeed (\c _ s -> Src.Fx c s)
                        |> andMap chompCommand
                        |> andMap (Advanced.symbol "," E.Effect)
                        |> andMap chompSubscription
                    ]
            )
        |> andThen (\m -> Advanced.symbol "}" E.Effect |> andThen (\_ -> succeed m))


chompCommand : P.Parser E.Module (A.Located Name.Name)
chompCommand =
    succeed identity
        |> andMap (Keyword.command_ E.Effect)
        |> andMap (Advanced.symbol "=" E.Effect)
        |> andMap (P.located (Var.upper E.Effect))


chompSubscription : P.Parser E.Module (A.Located Name.Name)
chompSubscription =
    succeed identity
        |> andMap (Keyword.subscription_ E.Effect)
        |> andMap (Advanced.symbol "=" E.Effect)
        |> andMap (P.located (Var.upper E.Effect))



-- IMPORTS


chompImports : List Src.Import -> P.Parser E.Module (List Src.Import)
chompImports initialImports =
    loop initialImports chompImportsHelp


chompImportsHelp : List Src.Import -> P.Parser E.Module (Step (List Src.Import) (List Src.Import))
chompImportsHelp revImports =
    oneOf
        [ chompImport |> andThen (\i -> succeed (Loop (i :: revImports)))
        , succeed (Done (List.reverse revImports))
        ]


chompImport : P.Parser E.Module Src.Import
chompImport =
    Keyword.import_ E.ImportStart
        |> andThen (\_ -> Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentName)
        |> andThen (\_ -> P.located (Var.moduleName E.ImportName))
        |> andThen
            (\name ->
                oneOf
                    [ freshLine E.ImportEnd |> map (\_ -> Src.Import name Nothing (Src.Explicit []))
                    , chompAs name
                    , chompExposing name Nothing
                    ]
            )


chompAs : A.Located Name.Name -> P.Parser E.Module Src.Import
chompAs name =
    Keyword.as_ E.ImportAs
        |> andThen (\_ -> Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentAlias)
        |> andThen (\_ -> Var.upper E.ImportAlias)
        |> andThen
            (\alias ->
                oneOf
                    [ freshLine E.ImportEnd |> map (\_ -> Src.Import name (Just alias) (Src.Explicit []))
                    , chompExposing name (Just alias)
                    ]
            )


chompExposing : A.Located Name.Name -> Maybe Name.Name -> P.Parser E.Module Src.Import
chompExposing name maybeAlias =
    Keyword.exposing_ E.ImportExposing
        |> andThen (\_ -> Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentExposingList)
        |> andThen (\_ -> exposing_)
        |> andThen
            (\exposed ->
                freshLine E.ImportEnd
                    |> map (\_ -> Src.Import name maybeAlias exposed)
            )



-- LISTING


exposing_ : P.Parser E.Exposing Src.Exposing
exposing_ =
    Advanced.symbol "(" E.ExposingStart
        |> andThen
            (\_ ->
                oneOf
                    [ Advanced.symbol ".." E.ExposingValue |> andThen (\_ -> Advanced.symbol ")" E.ExposingEnd) |> map (\_ -> Src.Open)
                    , loop [] exposingHelp |> map Src.Explicit
                    ]
            )


exposingHelp : List Src.Exposed -> P.Parser E.Exposing (Step (List Src.Exposed) (List Src.Exposed))
exposingHelp revExposed =
    oneOf
        [ Advanced.symbol "," E.ExposingEnd
            |> andThen (\_ -> chompExposed)
            |> andThen (\e -> succeed (Loop (e :: revExposed)))
        , Advanced.symbol ")" E.ExposingEnd
            |> andThen (\_ -> succeed (Done (List.reverse revExposed)))
        ]


chompExposed : P.Parser E.Exposing Src.Exposed
chompExposed =
    oneOf
        [ P.located (Var.lower E.ExposingValue) |> map Src.Lower
        , P.located (Advanced.symbol "(" E.ExposingValue |> andThen (\_ -> Symbol.operator E.ExposingOperator E.ExposingOperatorReserved) |> andThen (\op -> Advanced.symbol ")" E.ExposingOperatorRightParen |> map (\_ -> op))) |> map (\(A.At r o) -> Src.Operator r o)
        , P.located (Var.upper E.ExposingValue) |> andThen (\n -> privacy |> map (\p -> Src.Upper n p))
        ]


privacy : P.Parser E.Exposing Src.Privacy
privacy =
    oneOf
        [ Advanced.symbol "(..)" E.ExposingTypePrivacy
            |> map (\_ -> Src.Public A.one)
        ]
        |> oneOfWithFallback Src.Private
