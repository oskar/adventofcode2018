module Day05 exposing (solvers)

import Answer exposing (Answer, ProblemSolver)
import Set


isReacting : Char -> Char -> Bool
isReacting a b =
    if a == b then
        False

    else
        Char.toLower a == Char.toLower b


isUnitOfType : Char -> Char -> Bool
isUnitOfType unitType unit =
    Char.toLower unitType == Char.toLower unit


triggerReaction : List Char -> List Char -> List Char
triggerReaction reactedUnits sourceUnits =
    case sourceUnits of
        [] ->
            reactedUnits

        [ unit ] ->
            unit :: reactedUnits

        unitA :: unitB :: restOfUnits ->
            if isReacting unitA unitB then
                case reactedUnits of
                    [] ->
                        triggerReaction [] restOfUnits

                    retryUnit :: finishedReacted ->
                        triggerReaction finishedReacted (retryUnit :: restOfUnits)

            else
                triggerReaction (unitA :: reactedUnits) (unitB :: restOfUnits)


solveProblem1 : ProblemSolver
solveProblem1 input =
    input
        |> String.trim
        |> String.toList
        |> triggerReaction []
        |> List.length
        |> Answer.fromInt


solveProblem2 : ProblemSolver
solveProblem2 input =
    let
        polymer =
            input
                |> String.trim
                |> String.toList

        uniqueUnits =
            polymer
                |> List.map Char.toLower
                |> Set.fromList

        maybeShortestLength =
            uniqueUnits
                |> Set.foldl
                    (\unitToFilterOut lengths ->
                        (polymer
                            |> List.filter (not << isUnitOfType unitToFilterOut)
                            |> triggerReaction []
                            |> List.length
                        )
                            :: lengths
                    )
                    []
                |> List.minimum
    in
    maybeShortestLength
        |> Maybe.map Answer.fromInt
        |> Maybe.withDefault Answer.nope


solvers =
    ( solveProblem1, solveProblem2 )
