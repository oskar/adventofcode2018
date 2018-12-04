module Day03 exposing (solvers)

import Answer exposing (Answer, ProblemSolver)
import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser, symbol)


type alias Id =
    Int


type alias Claim =
    { id : Id
    , x : Int
    , y : Int
    , w : Int
    , h : Int
    }


type alias Coord =
    ( Int, Int )


type alias IdWithCoord =
    ( Id, Coord )


type alias IdWithArea =
    ( Id, Int )


type alias Overlap =
    Int


type alias Fabric =
    Dict Coord ( Id, Overlap )


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


coordsFromClaim : Claim -> List IdWithCoord
coordsFromClaim { id, x, y, w, h } =
    let
        coordFromIndex index =
            ( x + modBy w index, y + index // w )
    in
    -- Zero index based coordinates, hence -1
    List.range 0 (w * h - 1)
        |> List.map (coordFromIndex >> Tuple.pair id)


coordsFromClaims : List Claim -> List IdWithCoord
coordsFromClaims claims =
    claims
        |> List.map coordsFromClaim
        |> List.foldl (++) []


fabricFromCoords : List IdWithCoord -> Fabric
fabricFromCoords coords =
    coords
        |> List.foldl claimArea Dict.empty


claimArea : IdWithCoord -> Fabric -> Fabric
claimArea ( id, coord ) fabric =
    let
        updateOverlap maybeOverlap =
            case maybeOverlap of
                Just ( _, overlap ) ->
                    -- Will overwrite existing id, but it's ok, we only care about the ones who never gets overwritten
                    Just ( id, overlap + 1 )

                Nothing ->
                    Just ( id, 1 )
    in
    Dict.update coord updateOverlap fabric


countOverlapsMoreThan : Overlap -> Fabric -> Overlap
countOverlapsMoreThan n fabric =
    let
        isOverlapping ( _, ( _, overlap ) ) =
            overlap > n
    in
    fabric
        |> Dict.toList
        |> List.filter isOverlapping
        |> List.length


idsThatOverlapExactly : Overlap -> Fabric -> List Id
idsThatOverlapExactly n fabric =
    let
        isOverlapping ( _, overlap ) =
            overlap == n
    in
    fabric
        |> Dict.values
        |> List.filter isOverlapping
        |> List.map (\( id, _ ) -> id)


idWithAreaFromClaim : Claim -> IdWithArea
idWithAreaFromClaim { id, w, h } =
    ( id, w * h )


groupByValue : List comparable -> Dict comparable Int
groupByValue ids =
    let
        updateCount mv =
            case mv of
                Just count ->
                    Just (count + 1)

                Nothing ->
                    Just 1
    in
    List.foldl
        (\id dict -> Dict.update id updateCount dict)
        Dict.empty
        ids


noOverlap : Dict Id Int -> Dict Id Int -> List Id
noOverlap totalAreaById oneInchAreaById =
    oneInchAreaById
        |> Dict.filter
            (\id oneInchArea ->
                totalAreaById
                    |> Dict.get id
                    |> Maybe.map ((==) oneInchArea)
                    |> Maybe.withDefault False
            )
        |> Dict.keys


solveProblem1 : ProblemSolver
solveProblem1 input =
    let
        coords =
            coordsFromClaims (parse input)

        fabric =
            fabricFromCoords coords

        moreThanOneOverlaps =
            fabric
                |> countOverlapsMoreThan 1
    in
    moreThanOneOverlaps
        |> Answer.fromInt


solveProblem2 : ProblemSolver
solveProblem2 input =
    let
        claims : List Claim
        claims =
            parse input

        coords : List IdWithCoord
        coords =
            coordsFromClaims claims

        fabric : Fabric
        fabric =
            fabricFromCoords coords

        idsWithAtleastOneInchNotOverlapping : List Id
        idsWithAtleastOneInchNotOverlapping =
            idsThatOverlapExactly 1 fabric

        oneInchAreaById : Dict Id Int
        oneInchAreaById =
            groupByValue idsWithAtleastOneInchNotOverlapping

        totalAreaById : Dict Id Int
        totalAreaById =
            claims
                |> List.map idWithAreaFromClaim
                |> Dict.fromList

        maybeIdWithNoOverlap : Maybe Id
        maybeIdWithNoOverlap =
            noOverlap totalAreaById oneInchAreaById
                |> List.head
    in
    maybeIdWithNoOverlap
        |> Maybe.map Answer.fromInt
        |> Maybe.withDefault Answer.nope


solvers =
    ( solveProblem1, solveProblem2 )
