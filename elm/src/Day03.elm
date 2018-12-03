module Day03 exposing (solvers)

import Answer exposing (Answer, ProblemSolver)
import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser, symbol)


type alias Claim =
    { id : Int
    , x : Int
    , y : Int
    , w : Int
    , h : Int
    }


type alias Fabric =
    Dict ( Int, Int ) Int


type alias Coord =
    ( Int, Int )


parse : String -> List Claim
parse input =
    input
        |> String.lines
        |> List.map (Parser.run claimParser >> Result.toMaybe)
        |> List.filterMap identity


{-| Parses the following format #123 @ 3,2: 5x4
-}
claimParser : Parser Claim
claimParser =
    Parser.succeed Claim
        |. Parser.symbol "#"
        |= Parser.int
        |. Parser.symbol " @ "
        |= Parser.int
        |. Parser.symbol ","
        |= Parser.int
        |. Parser.symbol ": "
        |= Parser.int
        |. Parser.symbol "x"
        |= Parser.int


coordsFromClaim : Claim -> List Coord
coordsFromClaim { x, y, w, h } =
    let
        coordFromIndex index =
            ( x + modBy w index, y + index // w )
    in
    -- Zero index based coordinates, hence -1
    List.range 0 (w * h - 1)
        |> List.map coordFromIndex


fabricFromCoords : List Coord -> Fabric
fabricFromCoords coords =
    coords
        |> List.map (\coord -> ( coord, 1 ))
        |> Dict.fromList


claimArea : Coord -> Fabric -> Fabric
claimArea coord fabric =
    let
        updateOverlap maybeOverlap =
            case maybeOverlap of
                Just overlap ->
                    Just (overlap + 1)

                Nothing ->
                    Just 1
    in
    Dict.update coord updateOverlap fabric


countOverlapsMoreThan : Int -> Fabric -> Int
countOverlapsMoreThan n fabric =
    let
        isOverlapping ( _, overlaps ) =
            overlaps > n
    in
    fabric
        |> Dict.toList
        |> List.filter isOverlapping
        |> List.length


solveProblem1 : ProblemSolver
solveProblem1 input =
    let
        claimFabrics =
            parse input
                |> List.map coordsFromClaim
                |> List.foldl (++) []

        fabric =
            claimFabrics
                |> List.foldl claimArea Dict.empty

        moreThanOneOverlaps =
            fabric
                |> countOverlapsMoreThan 1
    in
    moreThanOneOverlaps
        |> Answer.fromInt


solveProblem2 : ProblemSolver
solveProblem2 input =
    -- TODO
    Answer.nope


solvers =
    ( solveProblem1, solveProblem2 )
