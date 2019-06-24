module Die.Production exposing (Color(..), attributes)

import Die
import Html
import Html.Attributes exposing (class)


type Color
    = Red
    | Yellow


attributes : Color -> Die.Model -> List (Html.Attribute msg)
attributes color die =
    let
        colorAttribute =
            case color of
                Red ->
                    class "has-text-danger"

                Yellow ->
                    class "has-text-warning"
    in
    colorAttribute :: [ class "fa-5x", iconAttributeForFace die.face ]


iconAttributeForFace : Die.Face -> Html.Attribute msg
iconAttributeForFace face =
    case face of
        Die.Face 1 ->
            class "fas fa-dice-one"

        Die.Face 2 ->
            class "fas fa-dice-two"

        Die.Face 3 ->
            class "fas fa-dice-three"

        Die.Face 4 ->
            class "fas fa-dice-four"

        Die.Face 5 ->
            class "fas fa-dice-five"

        Die.Face 6 ->
            class "fas fa-dice-six"

        _ ->
            class ""
