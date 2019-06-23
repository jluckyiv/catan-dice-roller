module Die.Event exposing
    ( Die
    , Msg
    , init
    , isRolling
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
    { face : Face
    , state : State
    }


init : Die
init =
    Die Barbarian NotRolling


type Face
    = Barbarian
    | YellowGate
    | BlueGate
    | GreenGate


type State
    = Rolling
    | NotRolling


roll : Die -> Die
roll die =
    { die | state = Rolling }


isRolling : Die -> Bool
isRolling die =
    case die.state of
        Rolling ->
            True

        NotRolling ->
            False



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
        (Random.weighted
            ( 3, Barbarian )
            [ ( 1, YellowGate ), ( 1, GreenGate ), ( 1, BlueGate ) ]
        )


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
    case die.face of
        Barbarian ->
            class "has-text-black"

        YellowGate ->
            class "has-text-warning"

        BlueGate ->
            class "has-text-info"

        GreenGate ->
            class "has-text-success"


iconAttribute : Die -> Html.Attribute msg
iconAttribute die =
    let
        fort =
            "fab fa-fort-awesome"
    in
    case die.face of
        Barbarian ->
            class "fas fa-skull-crossbones"

        YellowGate ->
            class fort

        BlueGate ->
            class fort

        GreenGate ->
            class fort
