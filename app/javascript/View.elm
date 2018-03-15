module View exposing (year, month, week)

import Html exposing (Html, div, a)
import Html.Attributes exposing (class)
import OnClickPage exposing (onClickPage)
import Date exposing (Month(..), Date)
import Date.Extra as Date exposing (Interval(..))
import RemoteData exposing (WebData, RemoteData(..))
import Msg exposing (Msg(..))
import Route
import Activity
import Zoom exposing (zoomIn)


year : Zoom.Model -> (Date -> Date -> WebData (List Activity.Model)) -> Html Msg
year zoom activityAccess =
    let
        normalizer = (Zoom.range zoom
            |> List.concatMap (\subZoom ->
                activityAccess subZoom.start subZoom.end
                    |> RemoteData.withDefault []
                    |> Activity.aggregateByType
                )
            ) |> Activity.durationNormalizer
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


monthOfYear : Zoom.Model -> (Date -> Date -> WebData (List Activity.Model)) -> (List Activity.Model -> List Activity.Model) -> Html Msg
monthOfYear zoom activities normalizer =
    div [ class "month" ]
        [ a (onClickPage (Route.Zoom zoom)) [ Html.text (zoom.start |> Date.month |> toString) ]
        , viewIfSuccess (activities zoom.start zoom.end) (Activity.aggregateByType >> normalizer >> Activity.viewStack)
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

dayOfWeekOfMonth : Zoom.Model -> (Date -> Date -> WebData (List Activity.Model)) -> Html Msg
dayOfWeekOfMonth zoom activities =
        div [ class "day" ]
        [ viewIfSuccess (activities zoom.start zoom.end) Activity.viewStack
        ]


dayOfWeek : Zoom.Model -> (Date -> Date -> WebData (List Activity.Model)) -> Html Msg
dayOfWeek zoom activities =
    div [ class "day" ]
        [ Html.text (zoom.start |> toString)
        , viewIfSuccess (activities zoom.start zoom.end)
            (\list ->
                div [] (List.map Activity.view list)
            )
        ]


viewIfSuccess : WebData (List Activity.Model) -> (List Activity.Model -> Html Msg) -> Html Msg
viewIfSuccess webdata html =
    case webdata of
        Success activities ->
            html activities

        Loading ->
            div [] [ Html.text "Loading" ]

        NotAsked ->
            div [] [ Html.text "NotAsked" ]

        Failure e ->
            div [] [ Html.text (e |> toString) ]
