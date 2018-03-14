module Main exposing (..)

import Html exposing (Html, div, span, a)
import Html.Attributes exposing (class, href)
import Date exposing (Date, Month(..))
import Date.Extra as Date
import Activity
import ActivityCache exposing (fetchActivities, accessActivities)
import RemoteData exposing (WebData, RemoteData(..))
import Route exposing (Route, parseLocation, ZoomLevel(..))
import Navigation exposing (Location)
import Msg exposing (Msg(..))
import OnClickPage exposing (onClickPage)
import Task


main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
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
    , route : Route
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        model =
            { activityCache = ActivityCache.initModel
            , zoomDate = Date.fromCalendarDate 2018 Jan 1
            , zoomLevel = Year
            , zoomActivity = Nothing
            , route = parseLocation location
            }
    in
        case model.route of
            Route.NotFound ->
                model ! []

            Route.Blank ->
                model ! [ Task.perform (\date -> NewPage (Route.Zoom Month date)) Date.now ]

            Route.Zoom _ _ ->
                update (OnLocationChange location) model


dateLimits : ZoomLevel -> Date -> ( Date, Date )
dateLimits zoomLevel zoomDate =
    case zoomLevel of
        Year ->
            ( Date.add Date.Year -1 (Date.floor Date.Month zoomDate), Date.ceiling Date.Month zoomDate )

        Month ->
            ( Date.add Date.Month -1 (Date.floor Date.Week zoomDate), Date.ceiling Date.Week zoomDate )

        Week ->
            ( Date.add Date.Week -1 (Date.floor Date.Day zoomDate), Date.ceiling Date.Day zoomDate )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLocationChange location ->
            let
                newRoute =
                    parseLocation location
            in
                case newRoute of
                    Route.Zoom level date ->
                        let
                            ( acModel, acMsg ) =
                                fetchActivities model.activityCache (dateLimits level date)
                        in
                            { model | zoomLevel = level, zoomDate = date, activityCache = acModel, route = newRoute }
                                ! [ acMsg |> Cmd.map UpdateActivityCache ]

                    Route.NotFound ->
                        ( { model | route = newRoute }, Cmd.none )

                    Route.Blank ->
                        ( { model | route = newRoute }, Cmd.none )

        NewPage page ->
            model ! [ Navigation.newUrl (Route.toString page) ]

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
    page model


page : Model -> Html Msg
page model =
    let
        ( startDate, endDate ) =
            dateLimits model.zoomLevel model.zoomDate
    in
        case model.route of
            Route.Zoom level date ->
                case level of
                    Year ->
                        div [ class "months" ]
                            (Date.range Date.Month 1 startDate endDate |> List.map (\date -> viewMonth date (accessActivities model.activityCache) OpenActivity))

                    Month ->
                        div [ class "weeks" ]
                            ((a (onClickPage (Route.Zoom Year model.zoomDate)) [ Html.text "Zoom out" ])
                                :: (Date.range Date.Week 1 startDate endDate |> List.map (\date -> viewWeek date (accessActivities model.activityCache) OpenActivity))
                            )

                    Week ->
                        div [ class "days" ]
                            (Date.range Date.Day 1 startDate endDate |> List.map (\date -> viewDay date (accessActivities model.activityCache) OpenActivity))

            Route.Blank ->
                div [] [ Html.text "Blank" ]

            Route.NotFound ->
                div [] [ Html.text "Not found" ]


viewMonth : Date -> (Date -> Date -> WebData (List Activity.Model)) -> (Activity.Model -> Msg) -> Html Msg
viewMonth date activityAccess openActivityMsg =
    let
        endDate =
            (Date.add Date.Month 1 date)

        activities =
            activityAccess date endDate
    in
        case activities of
            Success activities ->
                div [ class "month" ]
                    [ a (onClickPage (Route.Zoom Month endDate)) [ Html.text (date |> toString) ]
                    , Activity.viewTreemap activities
                    ]

            Loading ->
                div [] [ Html.text ((date |> toString) ++ "Loading") ]

            NotAsked ->
                div [] [ Html.text ((date |> toString) ++ "NotAsked") ]

            Failure e ->
                div [] [ Html.text (e |> toString) ]


viewWeek : Date -> (Date -> Date -> WebData (List Activity.Model)) -> (Activity.Model -> Msg) -> Html Msg
viewWeek date activityAccess openActivityMsg =
    let
        endDate =
            (Date.add Date.Week 1 date)

        activities =
            activityAccess date endDate
    in
        case activities of
            Success activities ->
                div [ class "week" ]
                    [a (onClickPage (Route.Zoom Week endDate)) [ Html.text (date |> toString) ]
                    , Activity.viewStack activities
                    ]

            Loading ->
                div [] [ Html.text ((date |> toString) ++ "Loading") ]

            NotAsked ->
                div [] [ Html.text ((date |> toString) ++ "NotAsked") ]

            Failure e ->
                div [] [ Html.text (e |> toString) ]


viewDay : Date -> (Date -> Date -> WebData (List Activity.Model)) -> (Activity.Model -> Msg) -> Html Msg
viewDay date activityAccess openActivityMsg =
    let
        endDate =
            (Date.add Date.Day 1 date)

        activities =
            activityAccess date endDate
    in
        case activities of
            Success activities ->
                div [ class "day" ]
                    ((span [] [ Html.text (date |> toString) ])
                        :: (List.map Activity.view activities)
                    )

            Loading ->
                div [] [ Html.text ((date |> toString) ++ "Loading") ]

            NotAsked ->
                div [] [ Html.text ((date |> toString) ++ "NotAsked") ]

            Failure e ->
                div [] [ Html.text (e |> toString) ]
