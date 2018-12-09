module Day04 exposing (solvers)

import Answer exposing (Answer, ProblemSolver)
import Dict exposing (Dict)
import Parser as P exposing ((|.), (|=), Parser)


type alias Event =
    { timestamp : Timestamp, action : EventType }


type EventType
    = GuardBeginsShift Int
    | FallsAsleep
    | WakesUp


type alias Timestamp =
    { year : Int, month : Int, day : Int, hour : Int, minute : Int }


type alias GuardState =
    { lastBeganShift : Timestamp
    , lastWentToSleep : Timestamp
    , sleptMinutes : List Int
    }


type alias GuardId =
    Int


parserEvents : Parser (List Event)
parserEvents =
    P.loop [] parserEventsHelp


parserEventsHelp allEvents =
    P.oneOf
        [ P.succeed (\anEvent -> P.Loop (anEvent :: allEvents))
            |= parserEvent
        , P.succeed ()
            |> P.map (\_ -> P.Done (List.reverse allEvents))
        ]


parserEvent : Parser Event
parserEvent =
    P.succeed Event
        |= parserTimestamp
        |= P.oneOf
            [ P.map GuardBeginsShift parserGuard
            , P.keyword "falls asleep"
                |. P.spaces
                |> P.map (\_ -> FallsAsleep)
            , P.keyword "wakes up"
                |. P.spaces
                |> P.map (\_ -> WakesUp)
            ]


parserGuard : Parser Int
parserGuard =
    P.succeed identity
        |. P.symbol "Guard #"
        |= P.int
        |. P.keyword " begins shift"
        |. P.spaces


parserTimestamp : Parser Timestamp
parserTimestamp =
    P.succeed Timestamp
        |. P.symbol "["
        |= P.int
        |. P.symbol "-"
        |= parserLeadingZeroInt
        |. P.symbol "-"
        |= parserLeadingZeroInt
        |. P.symbol " "
        |= parserLeadingZeroInt
        |. P.symbol ":"
        |= parserLeadingZeroInt
        |. P.symbol "] "


parserLeadingZeroInt : Parser Int
parserLeadingZeroInt =
    P.oneOf
        [ P.succeed identity
            |. P.symbol "0"
            |= P.int
        , P.int
        ]


sortEventByTimestamp : Event -> Event -> Order
sortEventByTimestamp a b =
    let
        compare : Int -> Int -> Order -> Order
        compare first second whenSame =
            if first < second then
                LT

            else if first > second then
                GT

            else
                whenSame
    in
    compare a.timestamp.year b.timestamp.year <|
        compare a.timestamp.month b.timestamp.month <|
            compare a.timestamp.day b.timestamp.day <|
                compare a.timestamp.hour b.timestamp.hour <|
                    compare a.timestamp.minute b.timestamp.minute EQ


zeroTimestamp : Timestamp
zeroTimestamp =
    Timestamp 0 1 1 0 0


setGuardBeginsShift guardId timestamp maybeState =
    case maybeState of
        Just state ->
            Just { state | lastBeganShift = timestamp }

        Nothing ->
            Just
                { lastBeganShift = timestamp
                , lastWentToSleep = zeroTimestamp
                , sleptMinutes = []
                }


setGuardFallAsleep guardId timestamp maybeState =
    case maybeState of
        Just state ->
            Just { state | lastWentToSleep = timestamp }

        Nothing ->
            Nothing


setGuardWakesUp guardId timestamp maybeState =
    case maybeState of
        Just state ->
            let
                sleptMinutes =
                    List.range state.lastWentToSleep.minute (timestamp.minute - 1)
            in
            Just { state | sleptMinutes = List.append state.sleptMinutes sleptMinutes }

        Nothing ->
            Nothing


guardStateFromEvents : Maybe GuardId -> List Event -> Dict GuardId GuardState -> Dict GuardId GuardState
guardStateFromEvents maybeGuardId events states =
    case events of
        [] ->
            states

        { action, timestamp } :: rest ->
            case maybeGuardId of
                Just guardId ->
                    case action of
                        GuardBeginsShift nextGuardId ->
                            Dict.update nextGuardId (setGuardBeginsShift nextGuardId timestamp) states
                                |> guardStateFromEvents (Just nextGuardId) rest

                        WakesUp ->
                            Dict.update guardId (setGuardWakesUp guardId timestamp) states
                                |> guardStateFromEvents (Just guardId) rest

                        FallsAsleep ->
                            Dict.update guardId (setGuardFallAsleep guardId timestamp) states
                                |> guardStateFromEvents (Just guardId) rest

                Nothing ->
                    case action of
                        GuardBeginsShift nextGuardId ->
                            guardStateFromEvents (Just nextGuardId) events states

                        _ ->
                            guardStateFromEvents maybeGuardId rest states


sleptTheMostGuardId : Dict GuardId GuardState -> Maybe GuardId
sleptTheMostGuardId guardState =
    Dict.toList guardState
        |> List.map (\( guardId, { sleptMinutes } ) -> ( guardId, List.length sleptMinutes ))
        |> List.sortBy Tuple.second
        |> List.map Tuple.first
        |> List.reverse
        |> List.head


mostSleptMinuteFromGuardState : GuardState -> Maybe Int
mostSleptMinuteFromGuardState guardState =
    countMinutes guardState.sleptMinutes
        |> Dict.toList
        |> List.sortBy Tuple.second
        |> List.map Tuple.first
        |> List.reverse
        |> List.head


countMinutes : List Int -> Dict Int Int
countMinutes minutes =
    let
        updateCount maybeMin =
            case maybeMin of
                Just min ->
                    Just (min + 1)

                Nothing ->
                    Just 1

        count min res =
            Dict.update min updateCount res
    in
    minutes
        |> List.foldl count Dict.empty


mapMostSleptMinuteWithCount : ( GuardId, GuardState ) -> ( GuardId, ( Int, Int ) )
mapMostSleptMinuteWithCount ( guardId, { sleptMinutes } ) =
    ( guardId
    , countMinutes sleptMinutes
        |> Dict.toList
        |> List.sortBy Tuple.second
        |> List.reverse
        |> List.head
        -- ( Minute, Count )
        |> Maybe.withDefault ( 0, 0 )
    )


solveProblem1 : ProblemSolver
solveProblem1 input =
    let
        events =
            P.run parserEvents input
                |> Result.withDefault []
                |> List.sortWith sortEventByTimestamp

        allGuardState =
            guardStateFromEvents Nothing events Dict.empty

        sleepyGuardId =
            sleptTheMostGuardId allGuardState
                |> Maybe.withDefault 0

        mostSleptMinute =
            Dict.get sleepyGuardId allGuardState
                |> Maybe.andThen mostSleptMinuteFromGuardState
                |> Maybe.withDefault 0

        answer =
            sleepyGuardId * mostSleptMinute
    in
    if answer == 0 then
        Answer.nope

    else
        Answer.fromInt answer


solveProblem2 : ProblemSolver
solveProblem2 input =
    let
        events =
            P.run parserEvents input
                |> Result.withDefault []
                |> List.sortWith sortEventByTimestamp

        allGuardState =
            guardStateFromEvents Nothing events Dict.empty

        ( guardId, ( minute, _ ) ) =
            allGuardState
                |> Dict.toList
                |> List.map mapMostSleptMinuteWithCount
                -- Sort by minute count
                |> List.sortBy (Tuple.second >> Tuple.second)
                |> List.reverse
                |> List.head
                |> Maybe.withDefault ( 0, ( 0, 0 ) )

        answer =
            guardId * minute
    in
    if answer == 0 then
        Answer.nope

    else
        Answer.fromInt answer


solvers =
    ( solveProblem1, solveProblem2 )
