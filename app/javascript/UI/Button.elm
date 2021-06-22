module UI.Button exposing (Color(..), Size(..), Tooltip(..), action, view, withAppearance, withAttributes, withTooltipPosition)

import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Json.Decode as Decode
import MonoIcons
import Svg exposing (Svg)
import UI.Util exposing (attributeIf, stopPropagationOnClick, viewMaybe)


type Size
    = Small
    | Medium
    | Wide


type Color
    = Primary
    | Regular
    | Subtle


type Tooltip
    = Top
    | Right
    | Bottom


type alias Config msg =
    { size : Size
    , color : Color
    , tooltip : Tooltip
    , label : String
    , onClick : msg
    , iconM : Maybe (String -> Svg msg)
    , attrs : List (Html.Attribute msg)
    }


action : String -> (String -> Svg msg) -> msg -> Config msg
action label icon onClick =
    { size = Medium
    , color = Regular
    , tooltip = Top
    , label = label
    , onClick = onClick
    , iconM = Just icon
    , attrs = [ class "button--basic" ]
    }


withAppearance : Size -> Color -> Tooltip -> Config msg -> Config msg
withAppearance size color tooltip config =
    { config | size = size, color = color, tooltip = tooltip }


withTooltipPosition : Tooltip -> Config msg -> Config msg
withTooltipPosition tooltip config =
    { config | tooltip = tooltip }


withAttributes : List (Html.Attribute msg) -> Config msg -> Config msg
withAttributes attrs config =
    { config | attrs = config.attrs ++ attrs }


view : Config msg -> Html msg
view { size, color, tooltip, label, onClick, iconM, attrs } =
    let
        iconFill =
            case color of
                Primary ->
                    "#ffffff"

                Regular ->
                    "#3d3d3d"

                Subtle ->
                    "var(--grey-900)"

        sizeClass =
            case size of
                Small ->
                    "button--small"

                Medium ->
                    "button--medium"

                Wide ->
                    "button--wide"
    in
    Html.button
        ([ class "button"
         , class sizeClass
         , attributeIf (color == Primary) (class "button--primary")
         , Html.Attributes.attribute "aria-label" label
         , style "text-align" "center"
         , stopPropagationOnClick (Decode.succeed onClick)
         ]
            ++ attrs
        )
        [ viewMaybe iconM (\icon -> MonoIcons.icon (icon iconFill))
        , Html.div
            [ class "tooltip"
            , attributeIf (tooltip == Right) (class "tooltip--right")
            , attributeIf (tooltip == Bottom) (class "tooltip--bottom")
            ]
            [ Html.text label ]
        ]
