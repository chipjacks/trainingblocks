module View.Zoom exposing (month, week, year)

import Html exposing (Html, div, a)
import Html.Attributes exposing (class)
import OnClickPage exposing (onClickPage)
import Date exposing (Month(..), Date)
import Date.Extra as Date exposing (Interval(..))
import RemoteData exposing (WebData, RemoteData(..))
import Msg exposing (Msg(..))
import Route
import Activity
import Block
import Zoom exposing (zoomIn)
import View.Block
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (width, height)


year : Zoom.Model -> (Date -> Date -> WebData (List Activity.Model)) -> Html Msg
year zoom activityAccess =
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


month : Zoom.Model -> (Date -> Date -> WebData (List Activity.Model)) -> Html Msg
month zoom activityAccess =
    div [ class "month" ]
        (  (headerOfMonth zoom)
        :: (Zoom.range zoom
                |> List.map (\subZoom -> weekOfMonth subZoom activityAccess)
           )
        )


week : Zoom.Model -> (Date -> Date -> WebData (List Activity.Model)) -> Html Msg
week zoom activityAccess =
    div [ class "week" ]
        (Zoom.range zoom
            |> List.map (\subZoom -> dayOfWeek subZoom (activityAccess))
        )



-- INTERNAL


monthOfYear : Zoom.Model -> (Date -> Date -> WebData (List Activity.Model)) -> (Block.Model -> Block.Model) -> Html Msg
monthOfYear zoom activities normalizer =
    div [ class "month" ] [ a (onClickPage (Route.Zoom zoom)) [ Html.text (zoom.start |> Date.month |> toString) ]
        , svg [ width "100%", height "100%" ]
            [ RemoteData.withDefault [] (activities zoom.start zoom.end)
                |> Activity.groupByType
                |> List.map (List.map (Block.initModel << Block.Activity))
                |> List.map Block.sum
                |> List.map (Block.scale 3 10)
                |> List.map normalizer
                |> Block.stack
                |> View.Block.view
            ]
    ]


headerOfMonth : Zoom.Model -> Html Msg
headerOfMonth zoom =
    div [ class "week header" ] 
        ( (div [ class "summary" ] [ ])
        ::(Zoom.range (Zoom.initModel Week zoom.end) |> List.map (\z -> div [ class "day" ] [Html.text (Date.dayOfWeek z.start |> toString)]))
        )


weekOfMonth : Zoom.Model -> (Date -> Date -> WebData (List Activity.Model)) -> Html Msg
weekOfMonth zoom activities =
    div [ class "week" ]
        ((a ((class "summary") :: (onClickPage (Route.Zoom zoom))) [ Html.text <| (zoom.start |> Date.toFormattedString "MMM ddd") ++ (zoom.end |> Date.toFormattedString " - ddd") ])
        :: (Zoom.range zoom
            |> List.map (\subZoom -> dayOfWeekOfMonth subZoom activities)
            )
        )

dayOfWeekOfMonth : Zoom.Model -> (Date -> Date -> WebData (List Activity.Model)) -> Svg Msg
dayOfWeekOfMonth zoom activities =
    div [ class "day" ] [
        svg [ width "100%", height "100%" ]
            [ RemoteData.withDefault [] (activities zoom.start zoom.end)
                |> List.map (Block.initModel << Block.Activity)
                |> List.map (Block.scale 1 10)
                |> List.map (Block.stack << Block.split 120)
                |> Block.stack
                |> View.Block.view
            ]
    ]


dayOfWeek : Zoom.Model -> (Date -> Date -> WebData (List Activity.Model)) -> Html Msg
dayOfWeek zoom activities =
    div [ class "day" ] [ Html.text (zoom.start |> Date.dayOfWeek |> toString)

        , svg [ width "100%", height "100%" ]
            [RemoteData.withDefault [] (activities zoom.start zoom.end)
                |> List.map (Block.initModel << Block.Activity)
                |> List.map (Block.scale 1 10)
                |> Block.stack -- TODO: plot on timeline instead of stacking
                |> View.Block.view
            ]
    ]