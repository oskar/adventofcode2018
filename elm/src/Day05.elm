module Day05 exposing (solvers)

import Answer exposing (Answer, ProblemSolver)
import Array exposing (Array)


isReacting : Char -> Char -> Bool
isReacting a b =
    if a == b then
        False

    else
        Char.toLower a == Char.toLower b


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
    Answer.nope


solvers =
    ( solveProblem1, solveProblem2 )
