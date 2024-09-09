module Optimize.Case exposing (optimize)

import AST.Canonical as Can
import AST.Optimized as Opt
import AssocList as Dict exposing (Dict)
import Data.Name as Name
import List.Extra as List
import Optimize.DecisionTree as DT
import Utils



-- OPTIMIZE A CASE EXPRESSION


optimize : Name.Name -> Name.Name -> List ( Can.Pattern, Opt.Expr ) -> Opt.Expr
optimize temp root optBranches =
    let
        ( patterns, indexedBranches ) =
            List.unzip (List.indexedMap indexify optBranches)

        decider =
            treeToDecider (DT.compile patterns)

        targetCounts =
            countTargets decider

        ( choices, maybeJumps ) =
            List.unzip (List.map (createChoices targetCounts) indexedBranches)
    in
    Opt.Case temp
        root
        (insertChoices (Dict.fromList choices) decider)
        (List.filterMap identity maybeJumps)


indexify : Int -> ( a, b ) -> ( ( a, Int ), ( Int, b ) )
indexify index ( pattern, branch ) =
    ( ( pattern, index )
    , ( index, branch )
    )



-- TREE TO DECIDER
--
-- Decision trees may have some redundancies, so we convert them to a Decider
-- which has special constructs to avoid code duplication when possible.


treeToDecider : DT.DecisionTree -> Opt.Decider Int
treeToDecider tree =
    case tree of
        DT.Match target ->
            Opt.Leaf target

        -- zero options
        DT.Decision _ [] Nothing ->
            Utils.crash "compiler bug, somehow created an empty decision tree"

        -- one option
        DT.Decision _ [ ( _, subTree ) ] Nothing ->
            treeToDecider subTree

        DT.Decision _ [] (Just subTree) ->
            treeToDecider subTree

        -- two options
        DT.Decision path [ ( test, successTree ) ] (Just failureTree) ->
            toChain path test successTree failureTree

        DT.Decision path [ ( test, successTree ), ( _, failureTree ) ] Nothing ->
            toChain path test successTree failureTree

        -- many options
        DT.Decision path edges Nothing ->
            let
                ( necessaryTests, fallback ) =
                    ( Utils.init edges, Tuple.second (Utils.last edges) )
            in
            Opt.FanOut
                path
                (List.map (Tuple.mapSecond treeToDecider) necessaryTests)
                (treeToDecider fallback)

        DT.Decision path edges (Just fallback) ->
            Opt.FanOut path (List.map (Tuple.mapSecond treeToDecider) edges) (treeToDecider fallback)


toChain : DT.Path -> DT.Test -> DT.DecisionTree -> DT.DecisionTree -> Opt.Decider Int
toChain path test successTree failureTree =
    let
        failure =
            treeToDecider failureTree
    in
    case treeToDecider successTree of
        Opt.Chain testChain success subFailure ->
            if failure == subFailure then
                Opt.Chain (( path, test ) :: testChain) success failure

            else
                Opt.Chain [ ( path, test ) ] success failure

        success ->
            Opt.Chain [ ( path, test ) ] success failure



-- INSERT CHOICES
--
-- If a target appears exactly once in a Decider, the corresponding expression
-- can be inlined. Whether things are inlined or jumps is called a "choice".


countTargets : Opt.Decider Int -> Dict Int Int
countTargets decisionTree =
    case decisionTree of
        Opt.Leaf target ->
            Dict.singleton target 1

        Opt.Chain _ success failure ->
            Utils.mapUnionWith (+) (countTargets success) (countTargets failure)

        Opt.FanOut _ tests fallback ->
            Utils.mapUnionsWith (+) (List.map countTargets (fallback :: List.map Tuple.second tests))


createChoices : Dict Int Int -> ( Int, Opt.Expr ) -> ( ( Int, Opt.Choice ), Maybe ( Int, Opt.Expr ) )
createChoices targetCounts ( target, branch ) =
    if Dict.get target targetCounts == Just 1 then
        ( ( target, Opt.Inline branch )
        , Nothing
        )

    else
        ( ( target, Opt.Jump target )
        , Just ( target, branch )
        )


insertChoices : Dict Int Opt.Choice -> Opt.Decider Int -> Opt.Decider Opt.Choice
insertChoices choiceDict decider =
    let
        go =
            insertChoices choiceDict
    in
    case decider of
        Opt.Leaf target ->
            Opt.Leaf (Utils.find target choiceDict)

        Opt.Chain testChain success failure ->
            Opt.Chain testChain (go success) (go failure)

        Opt.FanOut path tests fallback ->
            Opt.FanOut path (List.map (Tuple.mapSecond go) tests) (go fallback)
