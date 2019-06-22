module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, style)
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
    State


init : () -> ( Model, Cmd Msg )
init _ =
    ( NotRolling (Faces One One Barbarian)
    , Random.generate RuntimeGeneratedNewFaces roll
    )


type alias Faces =
    { redFace : DieFace
    , yellowFace : DieFace
    , eventFace : EventFace
    }


type State
    = Rolling Faces
    | NotRolling Faces


type DieFace
    = One
    | Two
    | Three
    | Four
    | Five
    | Six


type EventFace
    = Barbarian
    | YellowCastle
    | BlueCastle
    | GreenCastle


type Color
    = Black
    | Yellow
    | Green
    | Blue
    | Red



-- UPDATE


type Msg
    = UserClickedRollButton
    | RuntimeGeneratedNewFaces Faces
    | CssTransitionEnded


dieRoll : Random.Generator DieFace
dieRoll =
    Random.uniform One [ Two, Three, Four, Five, Six ]


eventRoll : Random.Generator EventFace
eventRoll =
    Random.weighted
        ( 3, Barbarian )
        [ ( 1, YellowCastle ), ( 1, GreenCastle ), ( 1, BlueCastle ) ]


roll : Random.Generator Faces
roll =
    Random.map3 Faces dieRoll dieRoll eventRoll


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( UserClickedRollButton, NotRolling faces_ ) ->
            ( Rolling faces_
            , Cmd.none
            )

        ( RuntimeGeneratedNewFaces faces, _ ) ->
            ( NotRolling faces
            , Cmd.none
            )

        ( CssTransitionEnded, _ ) ->
            ( model
            , Random.generate RuntimeGeneratedNewFaces roll
            )

        ( _, _ ) ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        NotRolling faces ->
            div []
                [ h1 [] [ eventToHtml model ]
                , h1 [ color Red ] [ dieToHtml model faces.redFace ]
                , h1 [ color Yellow ] [ dieToHtml model faces.yellowFace ]
                , h1 [] [ button [ onClick UserClickedRollButton ] [ text "Roll" ] ]
                ]

        Rolling faces ->
            div []
                [ h1 [] [ eventToHtml model ]
                , h1 [ color Red ] [ dieToHtml model faces.redFace ]
                , h1 [ color Yellow ] [ dieToHtml model faces.yellowFace ]
                , h1 [] [ button [] [ text "Rolling" ] ]
                ]


eventToHtml : State -> Html Msg
eventToHtml state =
    let
        class_ eventFace =
            case eventFace of
                Barbarian ->
                    barbarianClass

                _ ->
                    fortClass

        color_ eventFace =
            case eventFace of
                Barbarian ->
                    Black

                YellowCastle ->
                    Yellow

                BlueCastle ->
                    Blue

                GreenCastle ->
                    Green
    in
    case state of
        Rolling faces ->
            i
                [ class sizeClass
                , class (class_ faces.eventFace)
                , color (color_ faces.eventFace)
                , class "rotate rotating"
                , on "transitionend" (Decode.succeed CssTransitionEnded)
                ]
                []

        NotRolling faces ->
            i
                [ class sizeClass
                , class (class_ faces.eventFace)
                , color (color_ faces.eventFace)
                ]
                []


dieToHtml : State -> DieFace -> Html msg
dieToHtml state face =
    case state of
        Rolling _ ->
            i [ class (diceClass face), class sizeClass, class "rotate rotating" ] []

        NotRolling _ ->
            i [ class (diceClass face), class sizeClass ] []


fortClass : String
fortClass =
    "fab fa-fort-awesome"


sizeClass : String
sizeClass =
    "fa-5x"


barbarianClass : String
barbarianClass =
    "fas fa-skull-crossbones"


diceClass : DieFace -> String
diceClass face =
    let
        suffix =
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
    in
    "fas fa-dice-" ++ suffix


color : Color -> Html.Attribute msg
color color_ =
    style "color" (colorToString color_)


colorToString : Color -> String
colorToString color_ =
    case color_ of
        Black ->
            "black"

        Yellow ->
            "goldenrod"

        Green ->
            "green"

        Blue ->
            "blue"

        Red ->
            "indianred"
