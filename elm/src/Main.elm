module Main exposing (main)

import Answer exposing (Answer, ProblemSolver)
import Browser
import Day01
import Day02
import Day03
import Day04
import Dict exposing (Dict)
import Html exposing (button, div, h4, span, text, textarea)
import Html.Attributes as Attrib
import Html.Events exposing (onClick, onInput)


type alias DayNumber =
    Int


type alias Day =
    { input : String
    , answer1 : Answer
    , answer2 : Answer
    }


type ProblemPart
    = ProblemPart1
    | ProblemPart2


type alias Model =
    Dict DayNumber Day


type Msg
    = SetDayInput DayNumber String
    | SolveProblem DayNumber ProblemPart


type alias ProblemSolvers =
    Dict DayNumber ( ProblemSolver, ProblemSolver )


problemSolvers : Dict Int ( ProblemSolver, ProblemSolver )
problemSolvers =
    Dict.fromList
        [ ( 1, Day01.solvers )
        , ( 2, Day02.solvers )
        , ( 3, Day03.solvers )
        , ( 4, Day04.solvers )
        ]


toModel : ProblemSolvers -> Model
toModel solvers =
    solvers
        |> Dict.map
            (\_ ( _, _ ) ->
                { input = """[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:00] Guard #11 begins shift
"""

                --                     input = """[1518-11-01 00:00] Guard #10 begins shift
                -- [1518-11-01 00:05] falls asleep
                -- [1518-11-01 00:25] wakes up
                -- [1518-11-01 00:30] falls asleep
                -- [1518-11-01 00:55] wakes up
                -- [1518-11-01 23:58] Guard #99 begins shift
                -- [1518-11-02 00:40] falls asleep
                -- [1518-11-02 00:50] wakes up
                -- [1518-11-03 00:05] Guard #10 begins shift
                -- [1518-11-03 00:24] falls asleep
                -- [1518-11-03 00:29] wakes up
                -- [1518-11-04 00:02] Guard #99 begins shift
                -- [1518-11-04 00:36] falls asleep
                -- [1518-11-04 00:46] wakes up
                -- [1518-11-05 00:03] Guard #99 begins shift
                -- [1518-11-05 00:45] falls asleep
                -- [1518-11-05 00:55] wakes up
                -- """
                , answer1 = Answer.empty
                , answer2 = Answer.empty
                }
            )


setDayInput : String -> Day -> Day
setDayInput input day =
    { input = input
    , answer1 = Answer.empty
    , answer2 = Answer.empty
    }


setDayAnswer : Answer -> ProblemPart -> Day -> Day
setDayAnswer answer problemPart day =
    case problemPart of
        ProblemPart1 ->
            { day | answer1 = answer }

        ProblemPart2 ->
            { day | answer2 = answer }


solveDay : ProblemSolver -> ProblemPart -> Day -> Day
solveDay solver problemPart day =
    setDayAnswer (solver day.input) problemPart day


solverFromDayNumberAndProblem : DayNumber -> ProblemPart -> Maybe ProblemSolver
solverFromDayNumberAndProblem dayNumber problemPart =
    let
        solverByProblem ( problem1, problem2 ) =
            if problemPart == ProblemPart1 then
                problem1

            else
                problem2
    in
    Dict.get dayNumber problemSolvers
        |> Maybe.map solverByProblem


update msg model =
    case msg of
        SetDayInput dayNumber input ->
            ( Dict.update dayNumber (Maybe.map (setDayInput input)) model, Cmd.none )

        SolveProblem dayNumber problem ->
            case solverFromDayNumberAndProblem dayNumber problem of
                Just solver ->
                    ( Dict.update dayNumber
                        (Maybe.map (solveDay solver problem))
                        model
                    , Cmd.none
                    )

                Nothing ->
                    ( Dict.update dayNumber
                        (Maybe.map (setDayAnswer Answer.nope problem))
                        model
                    , Cmd.none
                    )


view : Model -> Browser.Document Msg
view model =
    { title = "adventofcode2018"
    , body =
        [ div
            []
            (model
                |> Dict.toList
                |> List.reverse
                |> List.map viewDay
            )
        ]
    }


viewDay : ( DayNumber, Day ) -> Html.Html Msg
viewDay ( dayNumber, day ) =
    let
        hasAnyInput =
            not (String.isEmpty day.input)

        answerText currentAnswer =
            if currentAnswer /= Answer.empty then
                Answer.toString currentAnswer

            else if hasAnyInput then
                "Press solve"

            else
                "Add input first"
    in
    div []
        [ h4 [] [ text ("Day " ++ String.fromInt dayNumber) ]
        , textarea
            [ Attrib.rows 6
            , Attrib.cols 40
            , onInput (SetDayInput dayNumber)
            , Attrib.value day.input
            ]
            []
        , div []
            [ button
                [ Attrib.disabled (not hasAnyInput)
                , onClick (SolveProblem dayNumber ProblemPart1)
                ]
                [ text "Solve 1" ]
            , span []
                [ text (answerText day.answer1) ]
            ]
        , div []
            [ button
                [ Attrib.disabled (not hasAnyInput)
                , onClick (SolveProblem dayNumber ProblemPart2)
                ]
                [ text "Solve 2" ]
            , span []
                [ text (answerText day.answer2) ]
            ]
        ]


init _ =
    ( toModel problemSolvers, Cmd.none )


main : Platform.Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
