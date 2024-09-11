module Type.Occurs exposing (occurs)

import Data.IO as IO exposing (IO)
import Data.Map as Dict
import Type.Type as Type
import Type.UnionFind as UF



-- OCCURS


occurs : UF.Variable -> IO Bool
occurs var =
    occursHelp [] var False


occursHelp : List UF.Variable -> UF.Variable -> Bool -> IO Bool
occursHelp seen var foundCycle =
    if List.member var seen then
        IO.pure True

    else
        UF.get var
            |> IO.bind
                (\(UF.Descriptor content _ _ _) ->
                    case content of
                        UF.FlexVar _ ->
                            IO.pure foundCycle

                        UF.FlexSuper _ _ ->
                            IO.pure foundCycle

                        UF.RigidVar _ ->
                            IO.pure foundCycle

                        UF.RigidSuper _ _ ->
                            IO.pure foundCycle

                        UF.Structure term ->
                            let
                                newSeen =
                                    var :: seen
                            in
                            case term of
                                UF.App1 _ _ args ->
                                    IO.foldrM (occursHelp newSeen) foundCycle args

                                UF.Fun1 a b ->
                                    IO.bind (occursHelp newSeen a)
                                        (occursHelp newSeen b foundCycle)

                                UF.EmptyRecord1 ->
                                    IO.pure foundCycle

                                UF.Record1 fields ext ->
                                    IO.bind (occursHelp newSeen ext) <|
                                        IO.foldrM (occursHelp newSeen) foundCycle (Dict.values fields)

                                UF.Unit1 ->
                                    IO.pure foundCycle

                                UF.Tuple1 a b maybeC ->
                                    case maybeC of
                                        Nothing ->
                                            IO.bind (occursHelp newSeen a)
                                                (occursHelp newSeen b foundCycle)

                                        Just c ->
                                            IO.bind (occursHelp newSeen a)
                                                (IO.bind (occursHelp newSeen b)
                                                    (occursHelp newSeen c foundCycle)
                                                )

                        UF.Alias _ _ args _ ->
                            IO.foldrM (occursHelp (var :: seen)) foundCycle (List.map Tuple.second args)

                        UF.Error ->
                            IO.pure foundCycle
                )
