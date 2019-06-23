module Die.Production exposing
    ( Color(..)
    , Die
    , Msg
    , init
    , roll
    , update
    , view
    )

import Html exposing (Html, i)
import Html.Attributes exposing (class)
import Html.Events exposing (on)
import Json.Decode as Decode
import Random



-- model


type alias Die =
    { color : Color
    , face : Face
    , state : State
    }


init : Color -> Die
init color_ =
    Die color_ One NotRolling


type Color
    = Red
    | Yellow


type Face
    = One
    | Two
    | Three
    | Four
    | Five
    | Six


type State
    = Rolling
    | NotRolling


roll : Die -> Die
roll die =
    { die | state = Rolling }



-- MSG


type Msg
    = UserStartedRoll
    | RollAnimationEnded
    | GotNewFace Face



-- UPDATE


update : Msg -> Die -> ( Die, Cmd Msg )
update msg die =
    case msg of
        UserStartedRoll ->
            ( { die | state = Rolling }
            , Cmd.none
            )

        RollAnimationEnded ->
            ( { die | state = NotRolling }
            , getNewFace
            )

        GotNewFace face ->
            ( { die | face = face }
            , Cmd.none
            )


getNewFace : Cmd Msg
getNewFace =
    Random.generate GotNewFace
        (Random.uniform One [ Two, Three, Four, Five, Six ])


view : Die -> Html Msg
view die =
    let
        transitionAttributes =
            case die.state of
                Rolling ->
                    [ on "transitionend" (Decode.succeed RollAnimationEnded)
                    ]

                NotRolling ->
                    []
    in
    i
        (transitionAttributes
            ++ [ sizeAttribute die
               , colorAttribute die
               , iconAttribute die
               , rollingAttribute die
               ]
        )
        []


sizeAttribute : Die -> Html.Attribute msg
sizeAttribute _ =
    class "fa-5x"


rollingAttribute : Die -> Html.Attribute msg
rollingAttribute { state } =
    case state of
        Rolling ->
            class "die rolling"

        NotRolling ->
            class ""


colorAttribute : Die -> Html.Attribute msg
colorAttribute die =
    case die.color of
        Yellow ->
            class "has-text-warning"

        Red ->
            class "has-text-danger"


iconAttribute : Die -> Html.Attribute msg
iconAttribute die =
    class ("fas fa-dice-" ++ toString die)



-- CONVERSION


color : Die -> String
color die =
    case die.color of
        Red ->
            "red"

        Yellow ->
            "yellow"


toInt : Die -> Int
toInt die =
    case die.face of
        One ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6


toString : Die -> String
toString die =
    case die.face of
        One ->
            "one"

        Two ->
            "two"

        Three ->
            "three"

        Four ->
            "four"

        Five ->
            "five"

        Six ->
            "six"
