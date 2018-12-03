module Day01 exposing (solvers)

import Answer exposing (..)
import Array exposing (Array)
import Set


parse : String -> List Int
parse input =
    String.split "\n" input
        |> List.map String.toInt
        |> List.filterMap identity


resultingFrequency : List Int -> Int
resultingFrequency freqs =
    List.sum freqs


findFirstDuplicateFrequency : Array Int -> Int
findFirstDuplicateFrequency changes =
    let
        findFirstDuplicateFrequencyHelper knownFreqs freq atChange =
            if Set.member freq knownFreqs then
                freq

            else
                let
                    updatedKnownFreqs =
                        Set.insert freq knownFreqs

                    nextFreq =
                        freq + (Array.get atChange changes |> Maybe.withDefault 0)

                    nextAtChange =
                        modBy (Array.length changes) (atChange + 1)
                in
                findFirstDuplicateFrequencyHelper updatedKnownFreqs nextFreq nextAtChange
    in
    if Array.isEmpty changes then
        0

    else
        findFirstDuplicateFrequencyHelper Set.empty 0 0


solveProblem1 : ProblemSolver
solveProblem1 input =
    parse input
        |> resultingFrequency
        |> toIntAnswer


solveProblem2 : ProblemSolver
solveProblem2 input =
    parse input
        |> Array.fromList
        |> findFirstDuplicateFrequency
        |> toIntAnswer


solvers =
    ( solveProblem1, solveProblem2 )
