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


fromByteString : SyntaxVersion -> ProjectType -> String -> Result E.Error Src.Module
fromByteString syntaxVersion projectType source =
    case P.fromByteString (chompModule syntaxVersion projectType) source of
        Ok modul ->
            checkModule syntaxVersion projectType modul

        Err deadEnds ->
            Err (toSyntaxError deadEnds)


toSyntaxError : List (P.DeadEnd P.Context P.Problem) -> E.Error
toSyntaxError deadEnds =
    -- This is a simplification. A real implementation would generate
    -- a more detailed error report based on the list of dead ends.
    case List.head deadEnds of
        Just deadEnd ->
            E.ParseError (E.ModuleProblem deadEnd.row deadEnd.col)

        Nothing ->
            E.ParseError (E.ModuleProblem 1 1)


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


type alias Module =
    { header : Maybe Header
    , imports : List Src.Import
    , infixes : List (A.Located Src.Infix)
    , decls : List Decl.Decl
    }


chompModule : SyntaxVersion -> ProjectType -> P.Parser Module
chompModule syntaxVersion projectType =
    chompHeader
        |> P.andThen
            (\header ->
                chompImports
                    (if isCore projectType then
                        []
                     else
                        Imports.defaults
                    )
                    |> P.andThen
                        (\imports ->
                            (if isKernel projectType then
                                chompInfixes []
                             else
                                P.succeed []
                            )
                                |> P.andThen
                                    (\infixes ->
                                        chompDecls syntaxVersion
                                            |> P.map
                                                (\decls ->
                                                    Module
                                                        header
                                                        imports
                                                        infixes
                                                        decls
                                                )
                                    )
                        )
            )


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


freshLine : P.Problem -> P.Parser ()
freshLine toFreshLineError =
    Space.chomp
        |> P.andThen (\_ -> Space.checkFreshLine toFreshLineError)


chompDecls : SyntaxVersion -> P.Parser (List Decl.Decl)
chompDecls syntaxVersion =
    P.loop [] (chompDeclsHelp syntaxVersion)


chompDeclsHelp : SyntaxVersion -> List Decl.Decl -> P.Parser (P.Step (List Decl.Decl) (List Decl.Decl))
chompDeclsHelp syntaxVersion decls =
    P.oneOfWithFallback
        [ Space.checkFreshLine (P.Problem_Decl P.DP_Start)
            |> P.andThen
                (\_ ->
                    Decl.declaration syntaxVersion
                        |> P.map (\decl -> P.Loop (decl :: decls))
                )
        ]
        (P.Done (List.reverse decls))


chompInfixes : List (A.Located Src.Infix) -> P.Parser (List (A.Located Src.Infix))
chompInfixes infixes =
    P.oneOfWithFallback
        [ Decl.infix_
            |> P.andThen (\binop -> chompInfixes (binop :: infixes))
        ]
        infixes


chompModuleDocCommentSpace : P.Parser (Result A.Region Src.Comment)
chompModuleDocCommentSpace =
    P.addLocation (freshLine (P.Problem_Module P.MP_FreshLine))
        |> P.andThen
            (\(A.At region ()) ->
                P.oneOfWithFallback
                    [ Space.docComment (P.Problem_Module P.MP_ImportStart) (P.Problem_Module P.MP_Space)
                        |> P.andThen
                            (\docComment ->
                                Space.chomp
                                    |> P.andThen (\_ -> Space.checkFreshLine (P.Problem_Module P.MP_FreshLine))
                                    |> P.map (\_ -> Ok docComment)
                            )
                    ]
                    (Err region)
            )


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


chompHeader : P.Parser (Maybe Header)
chompHeader =
    freshLine (P.Problem_Module P.MP_FreshLine)
        |> P.andThen (\_ -> P.getPosition)
        |> P.andThen
            (\start ->
                P.oneOfWithFallback
                    [ Keyword.module_ (P.Problem_Module P.MP_Problem)
                        |> P.andThen (\_ -> P.getPosition)
                        |> P.andThen
                            (\effectEnd ->
                                Space.chomp
                                    |> P.andThen (\_ -> P.addLocation (Var.moduleName (P.Problem_Module P.MP_ModuleName)))
                                    |> P.andThen
                                        (\name ->
                                            Space.chomp
                                                |> P.andThen (\_ -> Keyword.exposing_ (P.Problem_Module P.MP_Problem))
                                                |> P.andThen (\_ -> Space.chomp)
                                                |> P.andThen (\_ -> P.addLocation exposing_)
                                                |> P.andThen
                                                    (\exports ->
                                                        chompModuleDocCommentSpace
                                                            |> P.map
                                                                (\comment ->
                                                                    Just <|
                                                                        Header
                                                                            name
                                                                            (NoEffects (A.Region start effectEnd))
                                                                            exports
                                                                            comment
                                                                )
                                                    )
                                        )
                            )
                    , Keyword.port_ (P.Problem_Module P.MP_PortModuleProblem)
                        |> P.andThen (\_ -> Space.chomp)
                        |> P.andThen (\_ -> Keyword.module_ (P.Problem_Module P.MP_PortModuleProblem))
                        |> P.andThen (\_ -> P.getPosition)
                        |> P.andThen
                            (\effectEnd ->
                                Space.chomp
                                    |> P.andThen (\_ -> P.addLocation (Var.moduleName (P.Problem_Module P.MP_PortModuleName)))
                                    |> P.andThen
                                        (\name ->
                                            Space.chomp
                                                |> P.andThen (\_ -> Keyword.exposing_ (P.Problem_Module P.MP_PortModuleProblem))
                                                |> P.andThen (\_ -> Space.chomp)
                                                |> P.andThen (\_ -> P.addLocation exposing_)
                                                |> P.andThen
                                                    (\exports ->
                                                        chompModuleDocCommentSpace
                                                            |> P.map
                                                                (\comment ->
                                                                    Just <|
                                                                        Header
                                                                            name
                                                                            (Ports (A.Region start effectEnd))
                                                                            exports
                                                                            comment
                                                                )
                                                    )
                                        )
                            )
                    , Keyword.effect_ (P.Problem_Module P.MP_Effect)
                        |> P.andThen (\_ -> Space.chomp)
                        |> P.andThen (\_ -> Keyword.module_ (P.Problem_Module P.MP_Effect))
                        |> P.andThen (\_ -> P.getPosition)
                        |> P.andThen
                            (\effectEnd ->
                                Space.chomp
                                    |> P.andThen (\_ -> P.addLocation (Var.moduleName (P.Problem_Module P.MP_ModuleName)))
                                    |> P.andThen
                                        (\name ->
                                            Space.chomp
                                                |> P.andThen (\_ -> Keyword.where_ (P.Problem_Module P.MP_Effect))
                                                |> P.andThen (\_ -> Space.chomp)
                                                |> P.andThen (\_ -> chompManager)
                                                |> P.andThen
                                                    (\manager ->
                                                        Space.chomp
                                                            |> P.andThen (\_ -> Keyword.exposing_ (P.Problem_Module P.MP_Effect))
                                                            |> P.andThen (\_ -> Space.chomp)
                                                            |> P.andThen (\_ -> P.addLocation exposing_)
                                                            |> P.andThen
                                                                (\exports ->
                                                                    chompModuleDocCommentSpace
                                                                        |> P.map
                                                                            (\comment ->
                                                                                Just <|
                                                                                    Header name (Manager (A.Region start effectEnd) manager) exports comment
                                                                            )
                                                                )
                                                    )
                                        )
                            )
                    ]
                    Nothing
            )


chompManager : P.Parser Src.Manager
chompManager =
    P.succeed ()
        |. P.word1 '{' (P.Problem_Module P.MP_Effect)
        |> P.andThen (\_ -> spaces_em)
        |> P.andThen
            (\_ ->
                P.oneOf
                    [ chompCommand
                        |> P.andThen
                            (\cmd ->
                                spaces_em
                                    |> P.andThen
                                        (\_ ->
                                            P.oneOf
                                                [ P.succeed ()
                                                    |. P.word1 '}' (P.Problem_Module P.MP_Effect)
                                                    |> P.andThen (\_ -> spaces_em)
                                                    |> P.map (\_ -> Src.Cmd cmd)
                                                , P.succeed ()
                                                    |. P.word1 ',' (P.Problem_Module P.MP_Effect)
                                                    |> P.andThen (\_ -> spaces_em)
                                                    |> P.andThen (\_ -> chompSubscription)
                                                    |> P.andThen
                                                        (\sub ->
                                                            spaces_em
                                                                |> P.andThen (\_ -> P.word1 '}' (P.Problem_Module P.MP_Effect))
                                                                |> P.andThen (\_ -> spaces_em)
                                                                |> P.map (\_ -> Src.Fx cmd sub)
                                                        )
                                                ]
                                        )
                            )
                    , chompSubscription
                        |> P.andThen
                            (\sub ->
                                spaces_em
                                    |> P.andThen
                                        (\_ ->
                                            P.oneOf
                                                [ P.succeed ()
                                                    |. P.word1 '}' (P.Problem_Module P.MP_Effect)
                                                    |> P.andThen (\_ -> spaces_em)
                                                    |> P.map (\_ -> Src.Sub sub)
                                                , P.succeed ()
                                                    |. P.word1 ',' (P.Problem_Module P.MP_Effect)
                                                    |> P.andThen (\_ -> spaces_em)
                                                    |> P.andThen (\_ -> chompCommand)
                                                    |> P.andThen
                                                        (\cmd ->
                                                            spaces_em
                                                                |> P.andThen (\_ -> P.word1 '}' (P.Problem_Module P.MP_Effect))
                                                                |> P.andThen (\_ -> spaces_em)
                                                                |> P.map (\_ -> Src.Fx cmd sub)
                                                        )
                                                ]
                                        )
                            )
                    ]
            )


chompCommand : P.Parser (A.Located Name.Name)
chompCommand =
    Keyword.command_ (P.Problem_Module P.MP_Effect)
        |> P.andThen (\_ -> spaces_em)
        |> P.andThen (\_ -> P.word1 '=' (P.Problem_Module P.MP_Effect))
        |> P.andThen (\_ -> spaces_em)
        |> P.andThen (\_ -> P.addLocation (Var.upper (P.Problem_Module P.MP_Effect)))


chompSubscription : P.Parser (A.Located Name.Name)
chompSubscription =
    Keyword.subscription_ (P.Problem_Module P.MP_Effect)
        |> P.andThen (\_ -> spaces_em)
        |> P.andThen (\_ -> P.word1 '=' (P.Problem_Module P.MP_Effect))
        |> P.andThen (\_ -> spaces_em)
        |> P.andThen (\_ -> P.addLocation (Var.upper (P.Problem_Module P.MP_Effect)))


spaces_em : P.Parser ()
spaces_em =
    Space.chomp


chompImports : List Src.Import -> P.Parser (List Src.Import)
chompImports is =
    P.oneOfWithFallback
        [ chompImport
            |> P.andThen (\i -> chompImports (i :: is))
        ]
        (List.reverse is)


chompImport : P.Parser Src.Import
chompImport =
    Keyword.import_ (P.Problem_Module P.MP_ImportStart)
        |> P.andThen (\_ -> Space.chomp)
        |> P.andThen (\_ -> P.addLocation (Var.moduleName (P.Problem_Module P.MP_ImportName)))
        |> P.andThen
            (\name ->
                Space.chomp
                    |> P.andThen
                        (\_ ->
                            P.oneOf
                                [ freshLine (P.Problem_Module P.MP_ImportEnd)
                                    |> P.map (\_ -> Src.Import name Nothing (Src.Explicit []))
                                , Space.checkIndent (A.getEnd name) (P.Problem_Module P.MP_ImportEnd)
                                    |> P.andThen
                                        (\_ ->
                                            P.oneOf
                                                [ chompAs name
                                                , chompExposing name Nothing
                                                ]
                                        )
                                ]
                        )
            )


chompAs : A.Located Name.Name -> P.Parser Src.Import
chompAs name =
    Keyword.as_ (P.Problem_Module P.MP_ImportAs)
        |> P.andThen (\_ -> Space.chomp)
        |> P.andThen (\_ -> Var.upper (P.Problem_Module P.MP_ImportAlias))
        |> P.andThen
            (\alias ->
                Space.chomp
                    |> P.andThen
                        (\_ ->
                            P.oneOf
                                [ freshLine (P.Problem_Module P.MP_ImportEnd)
                                    |> P.map (\_ -> Src.Import name (Just alias) (Src.Explicit []))
                                , Space.checkIndent (A.sameAs name "") (P.Problem_Module P.MP_ImportEnd) -- dummy position
                                    |> P.andThen (\_ -> chompExposing name (Just alias))
                                ]
                        )
            )


chompExposing : A.Located Name.Name -> Maybe Name.Name -> P.Parser Src.Import
chompExposing name maybeAlias =
    Keyword.exposing_ (P.Problem_Module P.MP_ImportExposing)
        |> P.andThen (\_ -> Space.chomp)
        |> P.andThen (\_ -> exposing_)
        |> P.andThen
            (\exposed ->
                freshLine (P.Problem_Module P.MP_ImportEnd)
                    |> P.map (\_ -> Src.Import name maybeAlias exposed)
            )


exposing_ : P.Parser Src.Exposing
exposing_ =
    P.inContext (P.CtxNode P.NExposing)
        (P.succeed ()
            |. P.word1 '(' (P.Problem_Module (P.MP_Exposing P.EP_Start))
            |> P.andThen (\_ -> Space.chomp)
            |> P.andThen
                (\_ ->
                    P.oneOf
                        [ P.succeed Src.Open
                            |. P.word2 '.' '.' (P.Problem_Module (P.MP_Exposing P.EP_Value))
                            |> P.andThen (\_ -> Space.chomp)
                            |> P.andThen (\_ -> P.word1 ')' (P.Problem_Module (P.MP_Exposing P.EP_End)))
                        , chompExposed
                            |> P.andThen
                                (\exposed ->
                                    Space.chomp
                                        |> P.andThen (\_ -> P.loop [ exposed ] exposingHelp)
                                )
                        ]
                )
        )


exposingHelp : List Src.Exposed -> P.Parser (P.Step (List Src.Exposed) Src.Exposing)
exposingHelp revExposed =
    P.oneOf
        [ P.succeed ()
            |. P.word1 ',' (P.Problem_Module (P.MP_Exposing P.EP_End))
            |> P.andThen (\_ -> Space.chomp)
            |> P.andThen (\_ -> chompExposed)
            |> P.andThen
                (\exposed ->
                    Space.chomp
                        |> P.map (\_ -> P.Loop (exposed :: revExposed))
                )
        , P.succeed (Src.Explicit (List.reverse revExposed))
            |. P.word1 ')' (P.Problem_Module (P.MP_Exposing P.EP_End))
        ]


chompExposed : P.Parser Src.Exposed
chompExposed =
    P.oneOf
        [ Var.lower (P.Problem_Module (P.MP_Exposing P.EP_Value))
            |> P.andThen (\name -> P.addLocation (P.succeed name) |> P.map Src.Lower)
        , P.succeed ()
            |. P.word1 '(' (P.Problem_Module (P.MP_Exposing P.EP_Value))
            |> P.andThen (\_ -> Symbol.operator (P.Problem_Module (P.MP_Exposing P.EP_Operator)) (\op -> P.Problem_Module (P.MP_Exposing (P.EP_OperatorReserved op))))
            |> P.andThen
                (\op ->
                    P.addLocation (P.succeed op)
                        |> P.andThen (\locatedOp ->
                            P.succeed ()
                                |. P.word1 ')' (P.Problem_Module (P.MP_Exposing P.EP_OperatorRightParen))
                                |> P.map (\_ -> Src.Operator (A.toRegion locatedOp) (A.toValue locatedOp))
                           )
                )
        , Var.upper (P.Problem_Module (P.MP_Exposing P.EP_Value))
            |> P.andThen
                (\name ->
                    P.addLocation (P.succeed name)
                        |> P.andThen (\locatedName ->
                            Space.chomp
                                |> P.andThen (\_ -> privacy)
                                |> P.map (Src.Upper locatedName)
                           )
                )
        ]


privacy : P.Parser Src.Privacy
privacy =
    P.oneOfWithFallback
        [ P.succeed ()
            |. P.word1 '(' (P.Problem_Module (P.MP_Exposing P.EP_TypePrivacy))
            |> P.andThen (\_ -> Space.chomp)
            |> P.andThen (\_ -> P.addLocation (P.succeed ()))
            |> P.andThen
                (\start ->
                    P.succeed ()
                        |. P.word2 '.' '.' (P.Problem_Module (P.MP_Exposing P.EP_TypePrivacy))
                        |> P.andThen (\_ -> P.addLocation (P.succeed ()))
                        |> P.andThen
                            (\end ->
                                Space.chomp
                                    |> P.andThen (\_ -> P.word1 ')' (P.Problem_Module (P.MP_Exposing P.EP_TypePrivacy)))
                                    |> P.map (\_ -> Src.Public (A.mergeRegions (A.toRegion start) (A.toRegion end)))
                            )
                )
        ]
        Src.Private
