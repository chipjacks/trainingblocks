module View.Zoom exposing (viewMenu, view)

import Html exposing (Html, div, a, button, span)
import Html.Attributes exposing (class, id)
import OnClickPage exposing (onClickPage)
import Date exposing (Month(..), Date)
import Date.Extra as Date exposing (Interval(..))
import RemoteData exposing (WebData, RemoteData(..))
import Msg exposing (Msg(..))
import Route
import Activity exposing (Activity)
import Block
import Zoom exposing (zoomIn)
import View.Block
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (width, height)


viewMenu : Zoom.Model -> Html Msg
viewMenu zoom =
    div [ class "ui secondary menu" ]
        [ div [ class "ui simple dropdown item" ]
            [ Html.text (toString zoom.level)
            , Html.i [ class "dropdown icon" ] []
            , div [ class "menu" ]
                [ div ([ class "item" ] ++ (onClickPage (Route.Zoom { zoom | level = Year }))) [ Html.text "Year" ]
                , div ([ class "item" ] ++ (onClickPage (Route.Zoom { zoom | level = Month }))) [ Html.text "Month" ]
                , div ([ class "item" ] ++ (onClickPage (Route.Zoom { zoom | level = Week }))) [ Html.text "Week" ]
                ]
            ]
        , div [ class "ui simple dropdown item" ]
            [ Html.text (Zoom.string zoom)
            , Html.i [ class "dropdown icon" ] []
            , div [ class "menu" ]
                (List.range -3 3
                    |> List.reverse
                    |> List.map (\n -> Zoom.jump n zoom)
                    |> List.map (\z -> 
                        if z == zoom then
                            div ([ class "disabled item" ] ++ (onClickPage (Route.Zoom z)))
                                [ Html.text <| Zoom.string z ]
                        else
                            div ([ class "item" ] ++ (onClickPage (Route.Zoom z)))
                                [ Html.text <| Zoom.string z ]
                        )
                )
            ]
        , div [ class "right menu" ]
            [ div [ class "ui item" ]
                [ button ([ class "ui left attached basic icon button" ] ++ (onClickPage (Route.Zoom (Zoom.older zoom))))
                    [ Html.i [ class "arrow down icon" ] []
                    ]
                , button ([ class "ui right attached basic icon button" ] ++ (onClickPage (Route.Zoom (Zoom.newer zoom))))
                    [ Html.i [ class "arrow up icon" ] []
                    ]
                ]
            ]
        ]


view : Zoom.Model -> (Date -> Date -> WebData (List Activity)) -> Html Msg
view zoom activityAccess =
    case zoom.level of
        Year ->
            let
                normalizer = (Zoom.range zoom
                    |> List.concatMap (\subZoom ->
                        activityAccess subZoom.start subZoom.end
                            |> RemoteData.withDefault []
                            |> Activity.groupByType
                            |> List.map (List.map (Block.initModel << Block.Activity))
                            |> List.map Block.sum
                        )
                    ) |> Block.normalizer
            in
                div [ class "year" ]
                    (Zoom.range zoom
                        |> List.reverse
                        |> List.map (\subZoom -> monthOfYear subZoom activityAccess normalizer)
                    )

        Month ->
            div [ class "month" ]
                (  (headerOfMonth zoom)
                :: (Zoom.range zoom
                        |> List.reverse
                        |> List.map (\subZoom -> weekOfMonth subZoom activityAccess)
                )
                )

        Week ->
            div [ class "week", id "week-plot" ]
                [ div [ class "hours" ]
                    (Date.range Hour 1 (Date.floor Day zoom.start) (Date.add Day 1 (Date.floor Day zoom.start))
                        |> List.indexedMap (\i hr -> div [ class "hour" ] [ hr |> Date.toFormattedString "h" |> Html.text ])
                    )
                , div [ class "days" ]
                    (Zoom.range zoom 
                        |> List.reverse
                        |> List.map (\subZoom -> dayOfWeek subZoom activityAccess)
                    )
                ]

        _ ->
            div [] [ Html.text "Invalid interval" ]


-- INTERNAL


monthOfYear : Zoom.Model -> (Date -> Date -> WebData (List Activity)) -> (Block.Model -> Block.Model) -> Html Msg
monthOfYear zoom activities normalizer =
    div ([ class "month" ] ++  (onClickPage (Route.Zoom zoom)))
        [ div [ class "ui sub header" ] [ Html.text (zoom.start |> Date.month |> toString) ]
        , svg [ ]
            (List.concat
                (Zoom.range zoom 
                    |> List.indexedMap (\i z ->
                        (Zoom.range z
                            |> List.indexedMap (\j z2 ->
                                RemoteData.withDefault [] (activities z2.start z2.end)
                                    |> List.map (Block.initModel << Block.Activity)
                                    |> List.map (Block.scale (1/5) 5)
                                    |> List.map (Block.crop 30 25)
                                    |> List.map (Block.shift (j * 35) (i * 30))
                                    |> Block.list
                                    |> View.Block.view
                                )
                        )
                    )
                )
            )
    ]


headerOfMonth : Zoom.Model -> Html Msg
headerOfMonth zoom =
    div [ class "week header" ] 
        ( (div [ class "summary" ] [ ])
        ::(Zoom.range (Zoom.initModel Week zoom.end) |> List.map (\z -> div [ class "day" ] [Html.text (Date.dayOfWeek z.start |> toString)]))
        )


weekOfMonth : Zoom.Model -> (Date -> Date -> WebData (List Activity)) -> Html Msg
weekOfMonth zoom activities =
    div [ class "week" ]
        ((a ((class "summary") :: (onClickPage (Route.Zoom zoom))) [ Html.text <| (zoom.start |> Date.toFormattedString "MMM ddd") ++ (zoom.end |> Date.toFormattedString " - ddd") ])
        :: (Zoom.range zoom
            |> List.map (\subZoom -> dayOfWeekOfMonth subZoom activities)
            )
        )


dayOfWeekOfMonth : Zoom.Model -> (Date -> Date -> WebData (List Activity)) -> Html Msg
dayOfWeekOfMonth zoom activities =
    div [ class "day" ] [
        svg [ width "100%", height "100%" ]
            [RemoteData.withDefault [] (activities zoom.start zoom.end)
                |> List.map (Block.initModel << Block.Activity)
                |> List.map (Block.scale 0.5 10)
                |> List.map (Block.stack << Block.split 100)
                |> Block.list
                |> View.Block.view
            ]
    ]

dayOfWeek : Zoom.Model -> (Date -> Date -> WebData (List Activity)) -> Html Msg
dayOfWeek zoom activities =
    div [ class "day" ]
        [ span [ class "summary" ] [ zoom.start |> Date.toFormattedString "E" |> Html.text ]
        , svg [ ]
            (RemoteData.withDefault [] (activities zoom.start zoom.end)
                |> List.map (Block.initModel << Block.Activity)
                |> List.map (Block.scale (38 / 60) 10)
                |> List.map Block.plot
                |> List.map View.Block.view
            )
        ]