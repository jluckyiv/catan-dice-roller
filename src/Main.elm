module Main exposing (main)

import Browser
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



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model NotRolling (Faces Barbarian One One)
    , rollCmd GotNewFaces
    )



-- MODEL


type alias Model =
    { isRolling : IsRolling
    , faces : Faces
    }


type IsRolling
    = Rolling
    | NotRolling


type alias Faces =
    { eventFace : EventFace
    , redFace : ProductionFace
    , yellowFace : ProductionFace
    }


type EventFace
    = Barbarian
    | YellowGate
    | BlueGate
    | GreenGate


type ProductionFace
    = One
    | Two
    | Three
    | Four
    | Five
    | Six


type ProductionDieColor
    = YellowDie
    | RedDie



-- UPDATE


type Msg
    = UserClickedRollButton
    | RollingAnimationEnded
    | GotNewFaces Faces


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserClickedRollButton ->
            ( { model | isRolling = Rolling }
            , Cmd.none
            )

        RollingAnimationEnded ->
            ( model
            , rollCmd GotNewFaces
            )

        GotNewFaces faces ->
            ( { model | isRolling = NotRolling, faces = faces }
            , Cmd.none
            )


rollCmd : (Faces -> msg) -> Cmd msg
rollCmd toMsg =
    Random.generate toMsg roll


roll : Random.Generator Faces
roll =
    Random.map3 Faces eventRoll productionRoll productionRoll


eventRoll : Random.Generator EventFace
eventRoll =
    Random.weighted
        ( 3, Barbarian )
        [ ( 1, YellowGate ), ( 1, GreenGate ), ( 1, BlueGate ) ]


productionRoll : Random.Generator ProductionFace
productionRoll =
    Random.uniform One [ Two, Three, Four, Five, Six ]



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
                        [ div [ class "tile is-child" ] [ viewEventFace model ]
                        , div [ class "tile is-child" ] [ viewProductionFace RedDie model ]
                        , div [ class "tile is-child" ] [ viewProductionFace YellowDie model ]
                        , div [ class "tile is-child" ] [ viewRollButton model ]
                        ]
                    ]
                ]
            ]
        ]


viewEventFace : Model -> Html Msg
viewEventFace { isRolling, faces } =
    let
        transitionAttributes =
            case isRolling of
                Rolling ->
                    [ on "transitionend" (Decode.succeed RollingAnimationEnded)
                    ]

                NotRolling ->
                    []
    in
    viewFace isRolling
        (transitionAttributes
            ++ [ eventColorAttribute faces.eventFace
               , eventIconAttribute faces.eventFace
               ]
        )


viewProductionFace : ProductionDieColor -> Model -> Html msg
viewProductionFace dieColor { isRolling, faces } =
    let
        face =
            case dieColor of
                YellowDie ->
                    faces.yellowFace

                RedDie ->
                    faces.redFace
    in
    viewFace isRolling
        [ productionColorAttribute dieColor
        , productionIconAttribute face
        ]


viewFace : IsRolling -> List (Html.Attribute msg) -> Html msg
viewFace isRolling otherAttributes =
    i (otherAttributes ++ [ sizeAttribute, rollingAttribute isRolling ])
        []


viewRollButton : Model -> Html Msg
viewRollButton { isRolling } =
    button (rollButtonAttributes isRolling ++ [ class "button is-medium is-primary" ])
        [ text (rollButtonText isRolling) ]


rollButtonAttributes : IsRolling -> List (Html.Attribute Msg)
rollButtonAttributes isRolling =
    case isRolling of
        Rolling ->
            [ disabled True ]

        NotRolling ->
            [ disabled False, onClick UserClickedRollButton ]


rollButtonText : IsRolling -> String
rollButtonText isRolling =
    case isRolling of
        Rolling ->
            "Rolling"

        NotRolling ->
            "Roll"


sizeAttribute : Html.Attribute msg
sizeAttribute =
    class "fa-5x"


rollingAttribute : IsRolling -> Html.Attribute msg
rollingAttribute isRolling =
    case isRolling of
        Rolling ->
            class "die rolling"

        NotRolling ->
            class ""


eventColorAttribute : EventFace -> Html.Attribute msg
eventColorAttribute eventFace =
    case eventFace of
        Barbarian ->
            class "has-text-black"

        YellowGate ->
            class "has-text-warning"

        BlueGate ->
            class "has-text-info"

        GreenGate ->
            class "has-text-success"


eventIconAttribute : EventFace -> Html.Attribute msg
eventIconAttribute eventFace =
    let
        fort =
            "fab fa-fort-awesome"
    in
    case eventFace of
        Barbarian ->
            class "fas fa-skull-crossbones"

        YellowGate ->
            class fort

        BlueGate ->
            class fort

        GreenGate ->
            class fort


productionColorAttribute : ProductionDieColor -> Html.Attribute msg
productionColorAttribute color_ =
    case color_ of
        YellowDie ->
            class "has-text-warning"

        RedDie ->
            class "has-text-danger"


productionIconAttribute : ProductionFace -> Html.Attribute msg
productionIconAttribute face =
    class ("fas fa-dice-" ++ productionToString face)


productionToString : ProductionFace -> String
productionToString face =
    case face of
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
