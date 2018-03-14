module View exposing (year, month, week)

import Html exposing (Html, div, a)
import Html.Attributes exposing (class)
import OnClickPage exposing (onClickPage)
import Date exposing (Month(..), Date)
import RemoteData exposing (WebData, RemoteData(..))
import Msg exposing (Msg(..))
import Route
import Activity
import Zoom exposing (zoomIn)


year : Zoom.Model -> (Date -> Date -> WebData (List Activity.Model)) -> Html Msg
year zoom activityAccess =
    div [ class "months" ]
        (Zoom.range zoom
            |> List.map (\subZoom -> monthOfYear subZoom (activityAccess subZoom.start subZoom.end))
        )


month : Zoom.Model -> (Date -> Date -> WebData (List Activity.Model)) -> Html Msg
month zoom activityAccess =
    div [ class "weeks" ]
        (Zoom.range zoom
            |> List.map (\subZoom -> weekOfMonth subZoom (activityAccess subZoom.start subZoom.end))
        )


week : Zoom.Model -> (Date -> Date -> WebData (List Activity.Model)) -> Html Msg
week zoom activityAccess =
    div [ class "days" ]
        (Zoom.range zoom
            |> List.map (\subZoom -> dayOfWeek subZoom (activityAccess subZoom.start subZoom.end))
        )



-- INTERNAL


monthOfYear : Zoom.Model -> WebData (List Activity.Model) -> Html Msg
monthOfYear zoom activities =
    div [ class "month" ]
        [ a (onClickPage (Route.Zoom zoom)) [ Html.text (zoom.start |> toString) ]
        , viewIfSuccess activities Activity.viewTreemap
        ]


weekOfMonth : Zoom.Model -> WebData (List Activity.Model) -> Html Msg
weekOfMonth zoom activities =
    div [ class "week" ]
        [ a (onClickPage (Route.Zoom zoom)) [ Html.text (zoom.start |> toString) ]
        , viewIfSuccess activities Activity.viewStack
        ]


dayOfWeek : Zoom.Model -> WebData (List Activity.Model) -> Html Msg
dayOfWeek zoom activities =
    div [ class "day" ]
        [ Html.text (zoom.start |> toString)
        , viewIfSuccess activities
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
