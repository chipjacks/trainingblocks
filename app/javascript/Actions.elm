module Actions exposing (actionButton, viewActivityActions, viewFormActions, viewLapActions, viewMultiSelectActions, viewPopoverActions)

import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Json.Decode as Decode
import MonoIcons
import Msg exposing (..)
import Skeleton exposing (attributeIf, borderStyle, column, row, stopPropagationOnClick)
import Svg exposing (Svg)


viewActivityActions : Html Msg
viewActivityActions =
    row buttonGroupStyles
        [ actionButton Medium ClickedEdit MonoIcons.edit "Edit" False
        , actionButton Medium ClickedCopy MonoIcons.copy "Copy" False
        , actionButton Medium ClickedDelete MonoIcons.delete "Delete" False
        , actionButton Medium (ClickedShift True) MonoIcons.arrowUp "Shift Up" False
        , actionButton Medium (ClickedShift False) MonoIcons.arrowDown "Shift Down" False
        ]


viewLapActions : Bool -> Html Msg
viewLapActions isEditing =
    row
        (if isEditing then
            []

         else
            buttonGroupStyles
        )
        [ if isEditing then
            actionButton Medium ClickedEdit MonoIcons.check "Save Lap" True

          else
            actionButton Medium ClickedEdit MonoIcons.edit "Edit Lap" False
        , actionButton Medium ClickedCopy MonoIcons.copy "Copy" False
        , actionButton Medium ClickedDelete MonoIcons.delete "Delete" False
        , actionButton Medium (ClickedShift True) MonoIcons.arrowUp "Shift Up" False
        , actionButton Medium (ClickedShift False) MonoIcons.arrowDown "Shift Down" False
        ]


viewMultiSelectActions : Html Msg
viewMultiSelectActions =
    row buttonGroupStyles
        [ actionButton Medium ClickedDelete MonoIcons.delete "Delete" False
        , actionButton Medium ClickedGroup MonoIcons.folder "Group" False
        ]


viewFormActions : Html Msg
viewFormActions =
    row []
        [ actionButton Wide ClickedSubmit MonoIcons.check "Save" True
        , actionButton Medium ClickedClose MonoIcons.close "Close" False
        ]


viewPopoverActions : Html Msg
viewPopoverActions =
    column buttonGroupStyles
        [ actionButton Medium ClickedEdit MonoIcons.edit "Edit" False
        , actionButton Medium ClickedCopy MonoIcons.copy "Copy" False
        , actionButton Medium ClickedDelete MonoIcons.delete "Delete" False
        ]


buttonGroupStyles =
    [ class "button-group", style "border-radius" "5px", borderStyle "border", style "overflow" "hidden" ]


type ButtonSize
    = Small
    | Medium
    | Wide


actionButton : ButtonSize -> Msg -> (String -> Svg Msg) -> String -> Bool -> Html Msg
actionButton size onClickMsg icon labelStr primary =
    let
        iconFill =
            if primary then
                "#ffffff"

            else
                "#3d3d3d"

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
        , attributeIf primary (class "primary")
        , Html.Attributes.attribute "aria-label" labelStr
        , style "text-align" "center"
        , stopPropagationOnClick (Decode.succeed onClickMsg)
        ]
        [ MonoIcons.icon (icon iconFill) ]
