module Actions exposing (actionButton, viewActivityActions, viewAddAction, viewFormActions, viewLapActions, viewMultiSelectActions, viewPopoverActions)

import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Json.Decode as Decode
import MonoIcons
import Msg exposing (..)
import Svg exposing (Svg)
import UI.Layout exposing (column, row)
import UI.Util exposing (attributeIf, borderStyle, stopPropagationOnClick)


viewAddAction : Msg -> String -> Html Msg
viewAddAction msg label =
    row [ class "add-button", style "width" "fit-content", style "position" "relative" ]
        [ actionButton (ButtonConfig Small Subtle Right) msg MonoIcons.add label
        ]


viewActivityActions : Html Msg
viewActivityActions =
    row buttonGroupStyles
        [ actionButton defaultConfig ClickedEdit MonoIcons.edit "Edit"
        , actionButton defaultConfig ClickedCopy MonoIcons.copy "Copy"
        , actionButton defaultConfig ClickedDelete MonoIcons.delete "Delete"
        , actionButton defaultConfig (ClickedShift True) MonoIcons.arrowUp "Shift Up"
        , actionButton defaultConfig (ClickedShift False) MonoIcons.arrowDown "Shift Down"
        ]


viewLapActions : Bool -> Html Msg
viewLapActions isEditing =
    let
        config =
            if isEditing then
                { defaultConfig | tooltip = Bottom }

            else
                defaultConfig
    in
    row
        (if isEditing then
            [ style "position" "relative" ]

         else
            buttonGroupStyles
        )
        [ if isEditing then
            actionButton (ButtonConfig Medium Primary Bottom) ClickedEdit MonoIcons.check "Save Lap"

          else
            actionButton config ClickedEdit MonoIcons.edit "Edit Lap"
        , actionButton config ClickedCopy MonoIcons.copy "Copy"
        , actionButton config ClickedDelete MonoIcons.delete "Delete"
        , actionButton config (ClickedShift True) MonoIcons.arrowUp "Shift Up"
        , actionButton config (ClickedShift False) MonoIcons.arrowDown "Shift Down"
        ]


viewMultiSelectActions : Html Msg
viewMultiSelectActions =
    row buttonGroupStyles
        [ actionButton defaultConfig ClickedDelete MonoIcons.delete "Delete"
        , actionButton defaultConfig ClickedGroup MonoIcons.folder "Group"
        ]


viewFormActions : Html Msg
viewFormActions =
    row []
        [ actionButton (ButtonConfig Wide Primary Top) ClickedSubmit MonoIcons.check "Save"
        , actionButton defaultConfig ClickedClose MonoIcons.close "Close"
        ]


viewPopoverActions : Html Msg
viewPopoverActions =
    column buttonGroupStyles
        [ actionButton defaultConfig ClickedEdit MonoIcons.edit "Edit"
        , actionButton defaultConfig ClickedCopy MonoIcons.copy "Copy"
        , actionButton defaultConfig ClickedDelete MonoIcons.delete "Delete"
        ]


buttonGroupStyles =
    [ class "button-group", style "border-radius" "5px", borderStyle "border", style "position" "relative" ]


type ButtonSize
    = Small
    | Medium
    | Wide


type ButtonColor
    = Primary
    | Regular
    | Subtle


type TooltipPosition
    = Top
    | Right
    | Bottom


type alias ButtonConfig =
    { size : ButtonSize
    , color : ButtonColor
    , tooltip : TooltipPosition
    }


defaultConfig =
    ButtonConfig Medium Regular Top


actionButton : ButtonConfig -> Msg -> (String -> Svg Msg) -> String -> Html Msg
actionButton { size, color, tooltip } onClickMsg icon labelStr =
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
                    "small"

                Medium ->
                    "medium"

                Wide ->
                    "wide"
    in
    Html.button
        [ class "button expand basic"
        , class sizeClass
        , attributeIf (color == Primary) (class "primary")
        , Html.Attributes.attribute "aria-label" labelStr
        , style "text-align" "center"
        , stopPropagationOnClick (Decode.succeed onClickMsg)
        ]
        [ MonoIcons.icon (icon iconFill)
        , Html.div
            [ class "tooltip"
            , attributeIf (tooltip == Right) (class "right")
            , attributeIf (tooltip == Bottom) (class "bottom")
            ]
            [ Html.text labelStr ]
        ]
