module Die.Event exposing (attributes)

import Die
import Html
import Html.Attributes exposing (class)


attributes : Die.Model -> List (Html.Attribute msg)
attributes die =
    let
        gate =
            class "fab fa-fort-awesome"

        barbarian =
            [ class "fas fa-skull-crossbones", class "has-text-black" ]

        eventIconAttribute d =
            case d.face of
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
    in
    class "fa-5x" :: eventIconAttribute die
