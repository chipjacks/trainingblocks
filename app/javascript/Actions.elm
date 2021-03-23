module Actions exposing (viewFullToolbar, viewMultiSelectToolbar, viewPopoverActions)

import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import MonoIcons
import Msg exposing (..)
import Skeleton exposing (attributeIf, column, row)
import Svg exposing (Svg)


viewFullToolbar : Bool -> Html Msg
viewFullToolbar editing =
    row []
        [ if editing then
            toolbarButton ClickedSubmit MonoIcons.check "Save" True

          else
            toolbarButton ClickedEdit MonoIcons.edit "Edit" False
        , toolbarButton ClickedCopy MonoIcons.copy "Copy" False
        , toolbarButton ClickedDelete MonoIcons.delete "Delete" False
        , column [] []
        , toolbarButton (ClickedShift True) MonoIcons.arrowUp "Shift Up" False
        , toolbarButton (ClickedShift False) MonoIcons.arrowDown "Shift Down" False
        , column [] []
        , toolbarButton ClickedClose MonoIcons.close "Close" False
        ]


viewMultiSelectToolbar : Html Msg
viewMultiSelectToolbar =
    row []
        [ toolbarButton ClickedDelete MonoIcons.delete "Delete" False
        , toolbarButton ClickedGroup MonoIcons.folder "Group" False
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
