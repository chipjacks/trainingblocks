module Actions exposing (toolbarButton, viewActivityActions, viewFormActions, viewLapActions, viewMultiSelectActions, viewPopoverActions)

import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import MonoIcons
import Msg exposing (..)
import Skeleton exposing (attributeIf, column, row)
import Svg exposing (Svg)


viewActivityActions : Html Msg
viewActivityActions =
    row []
        [ toolbarButton ClickedEdit MonoIcons.edit "Edit" False
        , toolbarButton ClickedCopy MonoIcons.copy "Copy" False
        , toolbarButton ClickedDelete MonoIcons.delete "Delete" False
        , column [] []
        , toolbarButton (ClickedShift True) MonoIcons.arrowUp "Shift Up" False
        , toolbarButton (ClickedShift False) MonoIcons.arrowDown "Shift Down" False
        , column [] []
        , toolbarButton ClickedClose MonoIcons.close "Close" False
        ]


viewLapActions : Bool -> Html Msg
viewLapActions isEditing =
    row []
        [ toolbarButton ClickedCopy MonoIcons.copy "Copy" False
        , toolbarButton ClickedDelete MonoIcons.delete "Delete" False
        , toolbarButton (ClickedShift True) MonoIcons.arrowUp "Shift Up" False
        , toolbarButton (ClickedShift False) MonoIcons.arrowDown "Shift Down" False
        , if isEditing then
            toolbarButton ClickedEdit MonoIcons.check "Save" True

          else
            toolbarButton ClickedEdit MonoIcons.edit "Edit" False
        ]


viewMultiSelectActions : Html Msg
viewMultiSelectActions =
    row []
        [ toolbarButton ClickedDelete MonoIcons.delete "Delete" False
        , toolbarButton ClickedGroup MonoIcons.folder "Group" False
        ]


viewFormActions : Html Msg
viewFormActions =
    row []
        [ toolbarButton ClickedClose MonoIcons.close "Close" False
        , toolbarButton ClickedSubmit MonoIcons.check "Save" True
        ]


viewPopoverActions : Html Msg
viewPopoverActions =
    column []
        [ toolbarButton ClickedEdit MonoIcons.edit "Edit" False
        , toolbarButton ClickedCopy MonoIcons.copy "Copy" False
        , toolbarButton ClickedDelete MonoIcons.delete "Delete" False
        ]


toolbarButton : Msg -> (String -> Svg Msg) -> String -> Bool -> Html Msg
toolbarButton onClickMsg icon labelStr primary =
    let
        iconFill =
            if primary then
                "#ffffff"

            else
                "#3d3d3d"
    in
    Html.button
        [ class "button small expand"
        , attributeIf primary (class "primary")
        , Html.Attributes.attribute "aria-label" labelStr
        , style "margin-right" "0.2rem"
        , style "text-align" "center"
        , style "max-width" "3rem"
        , onClick onClickMsg
        ]
        [ MonoIcons.icon (icon iconFill) ]
