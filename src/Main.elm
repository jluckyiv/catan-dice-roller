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
    mainSection
        [ childTile (viewEventDie model.eventDie)
        , childTile (viewRedDie model.redDie)
        , childTile (viewYellowDie model.yellowDie)
        , childTile (viewRollButton model)
        ]


mainSection : List (Html Msg) -> Html Msg
mainSection contents =
    section [ class "section" ]
        [ div [ class "container" ]
            [ ancestorTile contents ]
        ]


ancestorTile : List (Html Msg) -> Html Msg
ancestorTile contents =
    div [ class "tile is-ancestor is-vertical" ]
        [ div [ class "tile" ]
            [ div [ class "tile is-parent is-vertical" ]
                contents
            ]
        ]


childTile : Html Msg -> Html Msg
childTile contents =
    div [ class "tile is-child" ] [ contents ]



-- DIE VIEW


viewDie :
    (Die.Msg -> Msg)
    -> List (Html.Attribute Die.Msg)
    -> Die.Model
    -> Html Msg
viewDie toMsg attributes die =
    Html.map toMsg (Die.view attributes die)


viewEventDie : Die.Model -> Html Msg
viewEventDie die =
    let
        attributes =
            [ sizeAttribute ] ++ eventDieAttributes die
    in
    viewDie GotEventDieMsg attributes die


viewRedDie : Die.Model -> Html Msg
viewRedDie die =
    let
        attributes =
            [ sizeAttribute, class "has-text-danger" ]
                ++ productionDieAttributes die
    in
    viewDie GotRedDieMsg attributes die


viewYellowDie : Die.Model -> Html Msg
viewYellowDie die =
    let
        attributes =
            [ sizeAttribute, class "has-text-warning" ]
                ++ productionDieAttributes die
    in
    viewDie GotYellowDieMsg attributes die


sizeAttribute : Html.Attribute msg
sizeAttribute =
    class "fa-5x"


eventDieAttributes : Die.Model -> List (Html.Attribute msg)
eventDieAttributes die =
    let
        gate =
            class "fab fa-fort-awesome"

        barbarian =
            [ class "fas fa-skull-crossbones", class "has-text-black" ]
    in
    case die.face of
        Die.Face 1 ->
            [ gate, class "has-text-info" ]

        Die.Face 2 ->
            [ gate, class "has-text-warning" ]

        Die.Face 3 ->
            [ gate, class "has-text-success" ]

        Die.Face 4 ->
            barbarian

        Die.Face 5 ->
            barbarian

        Die.Face 6 ->
            barbarian

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



-- ROLL BUTTON


viewRollButton : Model -> Html Msg
viewRollButton model =
    let
        isRolling =
            Die.isRolling model.eventDie

        attributes =
            class "button is-medium is-primary"
                :: [ disabled isRolling, onClick UserClickedRollButton ]

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
