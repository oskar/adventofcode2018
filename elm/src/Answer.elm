module Answer exposing (Answer, ProblemSolver, empty, fromInt, fromString, nope, toString)


type Answer
    = StringAnswer String
    | IntAnswer Int
    | NopeAnswer
    | EmptyAnswer


type alias ProblemSolver =
    String -> Answer


nope =
    NopeAnswer


empty =
    EmptyAnswer


fromInt : Int -> Answer
fromInt int =
    IntAnswer int


fromString : String -> Answer
fromString string =
    StringAnswer string


toString : Answer -> String
toString answer =
    case answer of
        IntAnswer i ->
            String.fromInt i

        StringAnswer s ->
            s

        NopeAnswer ->
            "MISSING ANSWER 😞"

        EmptyAnswer ->
            ""
