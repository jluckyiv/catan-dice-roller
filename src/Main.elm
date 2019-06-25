module Main exposing (main)

import Browser
import Die as Die
import Html exposing (..)
import Html.Attributes exposing (class, disabled, style)
import Html.Events exposing (..)
import Json.Decode as Decode
import Random



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { eventDie : Die.Model
    , redDie : Die.Model
    , yellowDie : Die.Model
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { eventDie = Die.init 6
      , redDie = Die.init 6
      , yellowDie = Die.init 6
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UserClickedRollButton
    | GotEventDieMsg Die.Msg
    | GotRedDieMsg Die.Msg
    | GotYellowDieMsg Die.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserClickedRollButton ->
            ( startRolling model
            , Cmd.none
            )

        GotEventDieMsg dieMsg ->
            let
                ( newDie, newCmd ) =
                    Die.update dieMsg model.eventDie
            in
            ( { model | eventDie = newDie }
            , Cmd.map GotEventDieMsg newCmd
            )

        GotRedDieMsg dieMsg ->
            let
                ( newDie, newCmd ) =
                    Die.update dieMsg model.redDie
            in
            ( { model | redDie = newDie }
            , Cmd.map GotRedDieMsg newCmd
            )

        GotYellowDieMsg dieMsg ->
            let
                ( newDie, newCmd ) =
                    Die.update dieMsg model.yellowDie
            in
            ( { model | yellowDie = newDie }
            , Cmd.map GotYellowDieMsg newCmd
            )


startRolling : Model -> Model
startRolling model =
    { model
        | eventDie = Die.roll model.eventDie
        , redDie = Die.roll model.redDie
        , yellowDie = Die.roll model.yellowDie
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    viewMain
        [ viewTile (viewEventDie model.eventDie)
        , viewTile (viewRedDie model.redDie)
        , viewTile (viewYellowDie model.yellowDie)
        , viewTile (viewRollButton model)
        ]


viewMain : List (Html Msg) -> Html Msg
viewMain contents =
    section [ class "section" ]
        [ div [ class "container" ]
            [ viewContainer contents ]
        ]


viewContainer : List (Html Msg) -> Html Msg
viewContainer contents =
    div [ class "tile is-ancestor is-vertical" ]
        [ div [ class "tile" ]
            [ div [ class "tile is-parent is-vertical" ]
                contents
            ]
        ]


viewTile : Html Msg -> Html Msg
viewTile contents =
    div [ class "tile is-child" ] [ contents ]



-- DIE VIEWS


viewEventDie : Die.Model -> Html Msg
viewEventDie die =
    eventDieAttributes die
        |> viewDie GotEventDieMsg die


viewRedDie : Die.Model -> Html Msg
viewRedDie die =
    productionDieAttributes die
        ++ colorAttributes Red
        |> viewDie GotRedDieMsg die


viewYellowDie : Die.Model -> Html Msg
viewYellowDie die =
    productionDieAttributes die
        ++ colorAttributes Yellow
        |> viewDie GotYellowDieMsg die


viewDie :
    (Die.Msg -> Msg)
    -> Die.Model
    -> List (Html.Attribute Die.Msg)
    -> Html Msg
viewDie toMsg die attributes =
    Html.map toMsg
        (Die.view (attributes ++ sizeAttributes) die)


sizeAttributes : List (Html.Attribute msg)
sizeAttributes =
    [ class "fa-5x" ]


eventDieAttributes : Die.Model -> List (Html.Attribute msg)
eventDieAttributes die =
    let
        gate =
            class "fab fa-fort-awesome"

        barbarian =
            class "fas fa-skull-crossbones"
    in
    case die.face of
        Die.Face 1 ->
            gate :: colorAttributes Blue

        Die.Face 2 ->
            gate :: colorAttributes Yellow

        Die.Face 3 ->
            gate :: colorAttributes Green

        Die.Face 4 ->
            barbarian :: colorAttributes Black

        Die.Face 5 ->
            barbarian :: colorAttributes Black

        Die.Face 6 ->
            barbarian :: colorAttributes Black

        _ ->
            [ class "" ]


productionDieAttributes : Die.Model -> List (Html.Attribute msg)
productionDieAttributes die =
    case die.face of
        Die.Face 1 ->
            [ class "fas fa-dice-one" ]

        Die.Face 2 ->
            [ class "fas fa-dice-two" ]

        Die.Face 3 ->
            [ class "fas fa-dice-three" ]

        Die.Face 4 ->
            [ class "fas fa-dice-four" ]

        Die.Face 5 ->
            [ class "fas fa-dice-five" ]

        Die.Face 6 ->
            [ class "fas fa-dice-six" ]

        _ ->
            [ class "" ]



-- COLORS


colorAttributes : Color -> List (Html.Attribute msg)
colorAttributes color =
    case color of
        Black ->
            [ class "has-text-black" ]

        Red ->
            [ class "has-text-danger" ]

        Yellow ->
            [ class "has-text-warning" ]

        Green ->
            [ class "has-text-success" ]

        Blue ->
            [ class "has-text-info" ]


type Color
    = Black
    | Red
    | Yellow
    | Green
    | Blue



-- ROLL BUTTON


viewRollButton : Model -> Html Msg
viewRollButton model =
    let
        isRolling =
            Die.isRolling model.eventDie

        attributes =
            [ class "button is-medium is-primary"
            , onClick UserClickedRollButton
            , disabled isRolling
            ]

        text_ =
            rollButtonText isRolling
    in
    button attributes [ text text_ ]


rollButtonText : Bool -> String
rollButtonText isRolling =
    if isRolling then
        "Rolling"

    else
        "Roll"
