module Main exposing (main)

import Browser
import Die as Die
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
    , Cmd.none {- rollCmd GotNewFaces -}
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
    div [ class "section" ]
        [ div [ class "container" ]
            [ div [ class "tile is-ancestor is-vertical" ]
                [ div [ class "tile" ]
                    [ div [ class "tile is-parent is-vertical" ]
                        [ div [ class "tile is-child" ] [ Html.map GotEventDieMsg (Die.view EventDie.attributes model.eventDie) ]
                        , div [ class "tile is-child" ] [ Html.map GotRedDieMsg (Die.view (ProductionDie.attributes ProductionDie.Red) model.redDie) ]
                        , div [ class "tile is-child" ] [ Html.map GotYellowDieMsg (Die.view (ProductionDie.attributes ProductionDie.Yellow) model.yellowDie) ]
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
            Die.isRolling model.eventDie
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
