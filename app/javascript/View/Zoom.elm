module View.Zoom exposing (month, week, year)

import Html exposing (Html, div, a)
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


year : Zoom.Model -> Maybe (Block.Event, Block.Model) -> (Date -> Date -> WebData (List Activity)) -> Html Msg
year zoom event activityAccess =
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
                |> List.map (\subZoom -> monthOfYear subZoom event activityAccess normalizer)
            )


month : Zoom.Model -> Maybe (Block.Event, Block.Model) -> (Date -> Date -> WebData (List Activity)) -> Html Msg
month zoom event activityAccess =
    div [ class "month" ]
        (  (headerOfMonth zoom)
        :: (Zoom.range zoom
                |> List.map (\subZoom -> weekOfMonth subZoom event activityAccess)
           )
        )


week : Zoom.Model -> Maybe (Block.Event, Block.Model) -> (Date -> Date -> WebData (List Activity)) -> Html Msg
week zoom event activityAccess =
    div [ class "week", id "week-plot" ]
        [ div [ class "hours" ]
            (Date.range Hour 1 (Date.floor Day zoom.start) (Date.add Day 1 (Date.floor Day zoom.start))
                |> List.indexedMap (\i hr -> div [ class "hour" ] [ hr |> Date.toFormattedString "h" |> Html.text ])
            )
        , div [ class "days" ]
            (Zoom.range zoom 
                |> List.indexedMap (\i dayZoom -> div [ class "day" ] [ dayZoom.start |> Date.toFormattedString "E" |> Html.text ])
            )
        , svg [ ] (plotBlocks zoom event activityAccess)
    ]


-- INTERNAL


monthOfYear : Zoom.Model -> Maybe (Block.Event, Block.Model) -> (Date -> Date -> WebData (List Activity)) -> (Block.Model -> Block.Model) -> Html Msg
monthOfYear zoom event activities normalizer =
    div [ class "month" ] [ a (onClickPage (Route.Zoom zoom)) [ Html.text (zoom.start |> Date.month |> toString) ]
        , svg [ width "100%", height "100%" ]
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
                                    |> View.Block.view event
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


weekOfMonth : Zoom.Model -> Maybe (Block.Event, Block.Model) -> (Date -> Date -> WebData (List Activity)) -> Html Msg
weekOfMonth zoom event activities =
    div [ class "week" ]
        ((a ((class "summary") :: (onClickPage (Route.Zoom zoom))) [ Html.text <| (zoom.start |> Date.toFormattedString "MMM ddd") ++ (zoom.end |> Date.toFormattedString " - ddd") ])
        :: (Zoom.range zoom
            |> List.map (\subZoom -> dayOfWeekOfMonth subZoom event activities)
            )
        )


dayOfWeekOfMonth : Zoom.Model -> Maybe (Block.Event, Block.Model) -> (Date -> Date -> WebData (List Activity)) -> Html Msg
dayOfWeekOfMonth zoom event activities =
    div [ class "day" ] [
        svg [ width "100%", height "100%" ]
            [RemoteData.withDefault [] (activities zoom.start zoom.end)
                |> List.map (Block.initModel << Block.Activity)
                |> List.map (Block.scale 0.5 10)
                |> List.map (Block.stack << Block.split 100)
                |> Block.list
                |> View.Block.view event
            ]
    ]


plotBlocks : Zoom.Model -> Maybe (Block.Event, Block.Model) -> (Date -> Date -> WebData (List Activity)) -> List (Svg Msg)
plotBlocks zoom event activities =
    RemoteData.withDefault [] (activities zoom.start zoom.end)
        |> List.map (Block.initModel << Block.Activity)
        |> List.map (Block.scale (10 / 6) 10)
        |> List.map Block.plot
        |> List.map (View.Block.view event)