module Main exposing (main)

import Browser
import Die.Event as EventDie
import Die.Production as ProductionDie
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
    { eventDie : EventDie.Die
    , redDie : ProductionDie.Die
    , yellowDie : ProductionDie.Die
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { eventDie = EventDie.init
      , redDie = ProductionDie.init ProductionDie.Red
      , yellowDie = ProductionDie.init ProductionDie.Yellow
      }
    , Cmd.none {- rollCmd GotNewFaces -}
    )



-- UPDATE


type Msg
    = UserClickedRollButton
    | EventDieMsg EventDie.Msg
    | RedDieMsg ProductionDie.Msg
    | YellowDieMsg ProductionDie.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserClickedRollButton ->
            ( startRolling model
            , Cmd.none
            )

        EventDieMsg eventMsg ->
            let
                ( updatedDie, eventCmd ) =
                    EventDie.update eventMsg model.eventDie
            in
            ( { model | eventDie = updatedDie }
            , Cmd.map EventDieMsg eventCmd
            )

        RedDieMsg productionMsg ->
            let
                ( updatedDie, productionCmd ) =
                    ProductionDie.update productionMsg model.redDie
            in
            ( { model | redDie = updatedDie }
            , Cmd.map RedDieMsg productionCmd
            )

        YellowDieMsg productionMsg ->
            let
                ( updatedDie, productionCmd ) =
                    ProductionDie.update productionMsg model.yellowDie
            in
            ( { model | yellowDie = updatedDie }
            , Cmd.map YellowDieMsg productionCmd
            )


startRolling : Model -> Model
startRolling model =
    { model
        | eventDie = EventDie.roll model.eventDie
        , redDie = ProductionDie.roll model.redDie
        , yellowDie = ProductionDie.roll model.yellowDie
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "section" ]
        [ div [ class "container" ]
            [ div [ class "tile is-ancestor is-vertical" ]
                [ div [ class "tile" ]
                    [ div [ class "tile is-parent is-vertical" ]
                        [ div [ class "tile is-child" ] [ Html.map EventDieMsg (EventDie.view model.eventDie) ]
                        , div [ class "tile is-child" ] [ Html.map RedDieMsg (ProductionDie.view model.redDie) ]
                        , div [ class "tile is-child" ] [ Html.map YellowDieMsg (ProductionDie.view model.yellowDie) ]
                        , div [ class "tile is-child" ] [ viewRollButton model ]
                        ]
                    ]
                ]
            ]
        ]


viewRollButton : Model -> Html Msg
viewRollButton model =
    let
        isRolling =
            EventDie.isRolling model.eventDie
    in
    button (rollButtonAttributes isRolling ++ [ class "button is-medium is-primary" ])
        [ text (rollButtonText isRolling) ]


rollButtonAttributes : Bool -> List (Html.Attribute Msg)
rollButtonAttributes isRolling =
    [ disabled isRolling, onClick UserClickedRollButton ]


rollButtonText : Bool -> String
rollButtonText isRolling =
    if isRolling then
        "Rolling"

    else
        "Roll"
