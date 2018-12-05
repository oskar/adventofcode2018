module Day04 exposing (solvers)

import Answer exposing (Answer, ProblemSolver)
import Parser as P exposing ((|.), (|=), Parser)


type alias Event =
    { timestamp : Timestamp, action : EventType }


type EventType
    = GuardBeginsShift Int
    | FallsAsleep
    | WakesUp


type alias Timestamp =
    { year : Int, month : Int, day : Int, hour : Int, minute : Int }


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


compare : Int -> Int -> Order -> Order
compare a b whenSame =
    if a < b then
        LT

    else if a > b then
        GT

    else
        whenSame


sortEventByTimestamp : Event -> Event -> Order
sortEventByTimestamp a b =
    compare a.timestamp.year b.timestamp.year <|
        compare a.timestamp.month b.timestamp.month <|
            compare a.timestamp.day b.timestamp.day EQ


solveProblem1 : ProblemSolver
solveProblem1 input =
    let
        events =
            P.run parserEvents input
                |> Result.withDefault []
                |> List.sortWith sortEventByTimestamp

        _ =
            Debug.log "events" events
    in
    Answer.nope


solveProblem2 : ProblemSolver
solveProblem2 input =
    Answer.nope


solvers =
    ( solveProblem1, solveProblem2 )
