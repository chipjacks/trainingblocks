module View.BlockEvent exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Svg exposing (svg)
import BlockEvent exposing (State(..))
import Block exposing (Data(..))
import View.Block
import Activity
import Date.Extra as Date
import Msg exposing (Msg(..))
import Mouse


view : BlockEvent.State -> Html Msg
view state =
    case state of
        Viewing mouseEvent blockModel ->
            div
                [ class "block-tooltip"
                , positionTooltip mouseEvent.pagePos mouseEvent.offsetPos blockModel.w
                ]
                [ viewTooltip blockModel.data ]

        Editing mouseEvent blockModel ->
            div
                [ class "block-tooltip"
                , positionTooltip mouseEvent.pagePos mouseEvent.offsetPos blockModel.w
                ]
                [ viewForm blockModel.data ]

        Creating mouseEvent activity ->
            div
                [ class "block-creator"
                , positionBlockCreator mouseEvent.pagePos mouseEvent.offsetPos
                ]
                [ svg
                    [ Mouse.onMove (\mouseEvent -> UpdateBlockEvent (BlockEvent.Stretch mouseEvent))
                    , Mouse.onUp (\mouseEvent -> UpdateBlockEvent (BlockEvent.EscapeCreate))
                    ]
                    [ Block.initModel (Activity activity)
                        |> Block.scale (38 / 60) 10
                        |> Block.plot
                        |> View.Block.view
                    ]
                ]

        None ->
            div [] []



-- INTERNAL


positionTooltip : ( Float, Float ) -> ( Float, Float ) -> Int -> Html.Attribute msg
positionTooltip pagePos offsetPos blockWidth =
    Html.Attributes.style
        [ ( "top", toString ((Tuple.second pagePos) - (Tuple.second offsetPos)) ++ "px" )
        , ( "left", toString ((Tuple.first pagePos) - (Tuple.first offsetPos) + (toFloat blockWidth) + 5) ++ "px" )
        ]


positionBlockCreator : ( Float, Float ) -> ( Float, Float ) -> Html.Attribute msg
positionBlockCreator pagePos offsetPos =
    Html.Attributes.style
        [ ( "top", toString ((Tuple.second pagePos) - (Tuple.second offsetPos)) ++ "px" )
        , ( "left", toString ((Tuple.first pagePos) - (Tuple.first offsetPos)) ++ "px" )
        ]


viewTooltip : Block.Data -> Html msg
viewTooltip data =
    case data of
        Activity activity ->
            div [ class "ui card" ]
                [ div [ class "content" ]
                    [ div [ class "header" ] [ Html.text activity.name ]
                    , div [ class "ui list" ]
                        [ div [ class "item" ] [ Html.text <| (toString activity.type_) ]
                        , div [ class "item" ] [ Html.text <| (Date.toFormattedString "h:mm a" activity.startDate) ]
                        , div [ class "item" ] [ Html.text <| ((toString (activity.duration // 60)) ++ " minutes") ]
                        , div [ class "item" ] [ Html.text <| Activity.miles activity ]
                        , div [ class "item" ] [ Html.text <| Activity.pace activity ]
                        ]
                    ]
                ]

        Blocks blocks ->
            div [] []


viewForm : Block.Data -> Html msg
viewForm data =
    case data of
        Activity activity ->
            div [ class "ui card" ]
                [ div [ class "content" ]
                    [ div [ class "header" ] [ Html.text activity.name ]
                    , Html.text "Editing"
                    ]
                ]

        Blocks blocks ->
            div [] []
