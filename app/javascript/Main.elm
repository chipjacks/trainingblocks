module Main exposing (..)

import Html exposing (Html, div, span, a)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Date exposing (Date, Month(..))
import Date.Extra as Date
import Task
import Activity
import ActivityCache exposing (fetchActivities, accessActivities)
import RemoteData exposing (WebData, RemoteData(..))


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { activityCache : ActivityCache.Model
    , zoomDate : Date
    , zoomLevel : ZoomLevel
    , zoomActivity : Maybe Activity.Model
    }


type ZoomLevel
    = Year
    | Month
    | Week


type Msg
    = Zoom ZoomLevel Date
    | OpenActivity Activity.Model
    | CloseActivity
    | UpdateActivityCache ActivityCache.Msg


init : ( Model, Cmd Msg )
init =
    let
        date =
            Date.fromCalendarDate 2018 Mar 1

        ac =
            ActivityCache.initModel

        model =
            { activityCache = ac
            , zoomDate = date
            , zoomLevel = Year
            , zoomActivity = Nothing
            }
    in
        model ! [ Task.perform (Zoom Year) Date.now ]


dateLimits : ZoomLevel -> Date -> ( Date, Date )
dateLimits zoomLevel zoomDate =
    case zoomLevel of
        Year ->
            ( Date.add Date.Month -12 zoomDate, zoomDate )

        Month ->
            ( Date.add Date.Month -1 zoomDate, zoomDate )

        Week ->
            ( Date.add Date.Week -1 zoomDate, zoomDate )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Zoom level date ->
            let
                ( acModel, acMsg ) =
                    fetchActivities model.activityCache (dateLimits level date)
            in
                { model | zoomLevel = level, zoomDate = date, activityCache = acModel }
                    ! [ acMsg |> Cmd.map UpdateActivityCache ]

        OpenActivity activity ->
            { model | zoomActivity = Just activity } ! []

        CloseActivity ->
            { model | zoomActivity = Nothing } ! []

        UpdateActivityCache subMsg ->
            { model | activityCache = ActivityCache.update subMsg model.activityCache } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model.zoomActivity of
        Just activity ->
            Activity.view activity

        Nothing ->
            let
                ( startDate, endDate ) =
                    dateLimits model.zoomLevel model.zoomDate
            in
                case model.zoomLevel of
                    Year ->
                        div [ class "months" ]
                            (Date.range Date.Month 1 startDate endDate |> List.map (\date -> viewMonth date (accessActivities model.activityCache) (Zoom Month) OpenActivity))

                    Month ->
                        div [ class "weeks" ]
                            ( (a [onClick (Zoom Year model.zoomDate)] [ Html.text "Zoom out" ])
                            :: (Date.range Date.Week 1 startDate endDate |> List.map (\date -> viewWeek date (accessActivities model.activityCache) (Zoom Week) OpenActivity))
                            )

                    Week ->
                        div [ class "days" ]
                            (Date.range Date.Day 1 startDate endDate |> List.map (\date -> viewDay date (accessActivities model.activityCache) (Zoom Week) OpenActivity))


viewMonth : Date -> (Date -> Date -> WebData (List Activity.Model)) -> (Date -> Msg) -> (Activity.Model -> Msg) -> Html Msg
viewMonth date activityAccess zoomInMsg openActivityMsg =
    let
        endDate = (Date.add Date.Month 1 date)
        activities =
            activityAccess date endDate
    in
        case activities of
            Success activities ->
                div [ class "month", onClick (zoomInMsg endDate) ]
                    [ span [] [ Html.text (date |> toString) ]
                    , Activity.viewTreemap activities
                    ]

            Loading ->
                div [] [ Html.text ((date |> toString) ++ "Loading") ]

            NotAsked ->
                div [] [ Html.text ((date |> toString) ++ "NotAsked") ]

            Failure e ->
                div [] [ Html.text (e |> toString) ]


viewWeek : Date -> (Date -> Date -> WebData (List Activity.Model)) -> (Date -> Msg) -> (Activity.Model -> Msg) -> Html Msg
viewWeek date activityAccess zoomInMsg openActivityMsg =
    let
        endDate = (Date.add Date.Week 1 date)
        activities =
            activityAccess date endDate
    in
        case activities of
            Success activities ->
                div [ class "week" , onClick (zoomInMsg endDate) ] 
                    ((span [] [ Html.text (date |> toString) ])
                    :: (List.map Activity.view activities))

            Loading ->
                div [] [ Html.text ((date |> toString) ++ "Loading") ]

            NotAsked ->
                div [] [ Html.text ((date |> toString) ++ "NotAsked") ]

            Failure e ->
                div [] [ Html.text (e |> toString) ]


viewDay : Date -> (Date -> Date -> WebData (List Activity.Model)) -> (Date -> Msg) -> (Activity.Model -> Msg) -> Html Msg
viewDay date activityAccess zoomInMsg openActivityMsg =
    let
        endDate = (Date.add Date.Day 1 date)
        activities =
            activityAccess date endDate
    in
        case activities of
            Success activities ->
                div [ class "day" ] 
                    ((span [] [ Html.text (date |> toString) ])
                    :: (List.map Activity.view activities))

            Loading ->
                div [] [ Html.text ((date |> toString) ++ "Loading") ]

            NotAsked ->
                div [] [ Html.text ((date |> toString) ++ "NotAsked") ]

            Failure e ->
                div [] [ Html.text (e |> toString) ]