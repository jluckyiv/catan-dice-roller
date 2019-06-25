module Die exposing
    ( Face(..)
    , Model
    , Msg
    , init
    , isRolling
    , roll
    , subscriptions
    , update
    , view
    )

import Html exposing (Html, i)
import Html.Attributes exposing (class)
import Html.Events exposing (on)
import Json.Decode as Decode
import Random
import Time



-- MODEL


init : Int -> Model
init faces =
    Model NotRolling faces (Face faces)


type alias Model =
    { state : State
    , faces : Int
    , face : Face
    }


type Face
    = Face Int


type State
    = Rolling
    | NotRolling


roll : Model -> Model
roll die =
    { die | state = Rolling }


isRolling : Model -> Bool
isRolling die =
    case die.state of
        Rolling ->
            True

        NotRolling ->
            False



-- MSG


type Msg
    = StartedRoll
    | EndedRollAnimation
    | GotNewFace Int
    | Ticked Time.Posix



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartedRoll ->
            ( { model | state = Rolling }, Cmd.none )

        EndedRollAnimation ->
            ( { model | state = NotRolling }, Cmd.none )

        GotNewFace face ->
            ( { model | face = Face face }, Cmd.none )

        Ticked _ ->
            ( model, getNewFace model )


getNewFace : Model -> Cmd Msg
getNewFace model =
    Random.generate GotNewFace (Random.int 1 model.faces)


view : List (Html.Attribute Msg) -> Model -> Html Msg
view attributes model =
    let
        transitionAttributes =
            case model.state of
                Rolling ->
                    [ on "transitionend" (Decode.succeed EndedRollAnimation)
                    , class "die rolling"
                    ]

                NotRolling ->
                    []
    in
    i (transitionAttributes ++ attributes) []


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Rolling ->
            Time.every 250 Ticked

        NotRolling ->
            Sub.none
