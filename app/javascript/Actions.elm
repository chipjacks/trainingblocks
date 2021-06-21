module Actions exposing (viewActivityActions, viewAddAction, viewFormActions, viewLapActions, viewMultiSelectActions, viewPopoverActions)

import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Json.Decode as Decode
import MonoIcons
import Msg exposing (..)
import Svg exposing (Svg)
import UI.Button as Button
import UI.Layout exposing (column, row)
import UI.Util exposing (attributeIf, borderStyle, stopPropagationOnClick)


viewAddAction : Msg -> String -> Html Msg
viewAddAction msg label =
    row [ class "add-button", style "width" "fit-content", style "position" "relative" ]
        [ Button.action label MonoIcons.add msg
            |> Button.withAppearance Button.Small Button.Subtle Button.Right
            |> Button.view
        ]


viewActivityActions : Html Msg
viewActivityActions =
    row buttonGroupStyles
        ([ Button.action "Edit" MonoIcons.edit ClickedEdit
         , Button.action "Copy" MonoIcons.copy ClickedCopy
         , Button.action "Delete" MonoIcons.delete ClickedDelete
         , Button.action "Shift Up" MonoIcons.arrowUp (ClickedShift True)
         , Button.action "Shift Down" MonoIcons.arrowDown (ClickedShift False)
         ]
            |> List.map Button.view
        )


viewLapActions : Bool -> Html Msg
viewLapActions isEditing =
    let
        tooltip =
            if isEditing then
                Button.Bottom

            else
                Button.Top
    in
    row
        (if isEditing then
            [ style "position" "relative" ]

         else
            buttonGroupStyles
        )
        ([ if isEditing then
            Button.action "Save Lap" MonoIcons.check ClickedEdit
                |> Button.withAppearance Button.Wide Button.Primary Button.Bottom

           else
            Button.action "Edit Lap" MonoIcons.edit ClickedEdit
         , Button.action "Copy" MonoIcons.copy ClickedCopy
         , Button.action "Delete" MonoIcons.delete ClickedDelete
         , Button.action "Shift Up" MonoIcons.arrowUp (ClickedShift True)
         , Button.action "Shift Down" MonoIcons.arrowDown (ClickedShift False)
         ]
            |> List.map (Button.withTooltipPosition tooltip >> Button.view)
        )


viewMultiSelectActions : Html Msg
viewMultiSelectActions =
    row buttonGroupStyles
        [ Button.action "Delete" MonoIcons.delete ClickedDelete
            |> Button.view
        , Button.action "Group" MonoIcons.folder ClickedGroup
            |> Button.view
        ]


viewFormActions : Html Msg
viewFormActions =
    row []
        [ Button.action "Save" MonoIcons.check ClickedSubmit
            |> Button.withAppearance Button.Wide Button.Primary Button.Top
            |> Button.view
        , Button.action "Close" MonoIcons.close ClickedClose
            |> Button.view
        ]


viewPopoverActions : Html Msg
viewPopoverActions =
    column buttonGroupStyles
        [ Button.action "Edit" MonoIcons.edit ClickedEdit
            |> Button.view
        , Button.action "Copy" MonoIcons.copy ClickedCopy
            |> Button.view
        , Button.action "Delete" MonoIcons.delete ClickedDelete
            |> Button.view
        ]


buttonGroupStyles =
    [ class "button-group", style "border-radius" "5px", borderStyle "border", style "position" "relative" ]
