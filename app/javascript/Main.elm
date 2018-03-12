module Main exposing (..)

import Html exposing (Html, div)
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


dateLimits : Model -> ( Date, Date )
dateLimits model =
    case model.zoomLevel of
        Year ->
            ( Date.add Date.Year -1 model.zoomDate, model.zoomDate )

        Month ->
            ( Date.add Date.Month -1 model.zoomDate, model.zoomDate )

        Week ->
            ( Date.add Date.Week -1 model.zoomDate, model.zoomDate )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Zoom level date ->
            let
                ( acModel, acMsg ) =
                    fetchActivities model.activityCache (dateLimits model)
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
                    dateLimits model
            in
                case model.zoomLevel of
                    Year ->
                        div [ onClick (Zoom Month endDate) ]
                            (Date.range Date.Month 1 startDate endDate |> List.map (\date -> viewMonth date (accessActivities model.activityCache) (Zoom Month) OpenActivity))

                    Month ->
                        div [ onClick (Zoom Week endDate) ]
                            (Date.range Date.Week 1 startDate endDate |> List.map (\date -> viewMonth date (accessActivities model.activityCache) (Zoom Week) OpenActivity))

                    Week ->
                        div []
                            (Date.range Date.Day 1 startDate endDate |> List.map (\date -> viewMonth date (accessActivities model.activityCache) (Zoom Week) OpenActivity))


viewMonth : Date -> (Date -> Date -> WebData (List Activity.Model)) -> (Date -> Msg) -> (Activity.Model -> Msg) -> Html Msg
viewMonth date activityAccess zoomInMsg openActivityMsg =
    let
        activities =
            activityAccess date (Date.add Date.Month 1 date)
    in
        case activities of
            Success activities ->
                Activity.viewTreemap activities

            Loading ->
                div [] [ Html.text "Loading" ]

            NotAsked ->
                div [] [ Html.text "NotAsked" ]

            Failure e ->
                div [] [ Html.text (e |> toString) ]
