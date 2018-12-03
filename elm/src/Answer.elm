module Answer exposing (Answer, ProblemSolver, toIntAnswer, toString, toStringAnswer)


type Answer
    = StringAnswer String
    | IntAnswer Int


type alias ProblemSolver =
    String -> Answer


toIntAnswer : Int -> Answer
toIntAnswer int =
    IntAnswer int


toStringAnswer : String -> Answer
toStringAnswer string =
    StringAnswer string


toString : Answer -> String
toString answer =
    case answer of
        IntAnswer i ->
            String.fromInt i

        StringAnswer s ->
            s
