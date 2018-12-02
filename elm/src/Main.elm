module Main exposing (main)

import Browser
import Day01
import Dict exposing (Dict)
import Html exposing (button, div, h4, span, text, textarea)
import Html.Attributes as Attrib
import Html.Events exposing (onClick, onInput)
import Solver exposing (Answer, Input, Solver)


type alias DayNumber =
    Int


type alias Day =
    { input : Input
    , answer1 : Answer
    , answer2 : Answer
    }


type Problem
    = Problem1
    | Problem2


type alias Model =
    Dict DayNumber Day


type Msg
    = SetDayInput DayNumber Input
    | SolveProblem DayNumber Problem


type alias ProblemSolvers =
    Dict DayNumber ( Input, Solver, Solver )


problemSolvers : ProblemSolvers
problemSolvers =
    Dict.fromList
        [ ( 1, ( "+3\n+3\n4\n-2\n-4", Day01.solveProblem1, Day01.solveProblem2 ) )
        ]


toModel : ProblemSolvers -> Model
toModel solvers =
    solvers
        |> Dict.map
            (\_ ( input, _, _ ) ->
                { input = input, answer1 = "", answer2 = "" }
            )


setDayInput : Input -> Day -> Day
setDayInput input day =
    { input = input, answer1 = "", answer2 = "" }


setDayAnswer : Answer -> Problem -> Day -> Day
setDayAnswer answer problem day =
    case problem of
        Problem1 ->
            { day | answer1 = answer }

        Problem2 ->
            { day | answer2 = answer }


solveDay : Solver -> Problem -> Day -> Day
solveDay solver problem day =
    setDayAnswer (solver day.input) problem day


solverFromDayNumberAndProblem : DayNumber -> Problem -> Maybe Solver
solverFromDayNumberAndProblem dayNumber problem =
    let
        solverByProblem ( _, solveProblem1, solveProblem2 ) =
            if problem == Problem1 then
                solveProblem1

            else
                solveProblem2
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
                        (Maybe.map (setDayAnswer "MISSING SOLVER" problem))
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
            if not (String.isEmpty currentAnswer) then
                currentAnswer

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
                , onClick (SolveProblem dayNumber Problem1)
                ]
                [ text "Solve 1" ]
            , span []
                [ text (answerText day.answer1) ]
            ]
        , div []
            [ button
                [ Attrib.disabled (not hasAnyInput)
                , onClick (SolveProblem dayNumber Problem2)
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
