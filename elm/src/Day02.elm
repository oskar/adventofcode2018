module Day02 exposing (solvers)

import Answer exposing (Answer, ProblemSolver)
import Dict


parse : String -> List String
parse input =
    String.split "\n" input


hasExactlyN : Int -> String -> Bool
hasExactlyN n str =
    let
        updateSeen maybeCount =
            case maybeCount of
                Just count ->
                    Just (count + 1)

                Nothing ->
                    Just 1
    in
    String.toList str
        |> List.foldl (\ch dict -> Dict.update ch updateSeen dict) Dict.empty
        |> Dict.values
        |> List.member n


diff : String -> String -> String
diff strA strB =
    let
        keepIfSame a b =
            if a == b then
                Just a

            else
                Nothing
    in
    List.map2 keepIfSame (String.toList strA) (String.toList strB)
        |> List.filterMap identity
        |> String.fromList


diffByOne : String -> String -> Bool
diffByOne strA strB =
    let
        len =
            min (String.length strA) (String.length strB)

        isOneLessInLength a =
            len - String.length a == 1
    in
    isOneLessInLength (diff strA strB)


commonLetters : List String -> Maybe String
commonLetters ids =
    case ids of
        [ a, b ] ->
            Just (diff a b)

        _ ->
            Nothing


offByOne ids =
    let
        isOffByOne id1 =
            List.any (\id2 -> diffByOne id1 id2) ids
    in
    List.filter isOffByOne ids


solveProblem1 : String -> Answer
solveProblem1 input =
    let
        lines =
            input |> parse

        nbOfTwos =
            lines
                |> List.filter (hasExactlyN 2)
                |> List.length

        nbOfThrees =
            lines
                |> List.filter (hasExactlyN 3)
                |> List.length

        checksum =
            nbOfTwos * nbOfThrees
    in
    checksum |> Answer.fromInt


solveProblem2 : String -> Answer
solveProblem2 input =
    let
        ids =
            parse input

        fabricId =
            ids
                |> offByOne
                |> commonLetters
    in
    fabricId
        |> Maybe.map Answer.fromString
        |> Maybe.withDefault Answer.nope


solvers =
    ( solveProblem1, solveProblem2 )
