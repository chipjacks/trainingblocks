module Main exposing (..)

import Html exposing (Html, div)
import Date exposing (Date, Month(..))
import Date.Extra as Date exposing (Interval(..))
import Activity
import ActivityCache exposing (fetchActivities, accessActivities)
import Route exposing (Route, parseLocation)
import Navigation exposing (Location)
import Msg exposing (Msg(..))
import Task
import Zoom
import View.Zoom
import Dom.Scroll
import Update.Extra exposing (filter, addCmd)
import Block


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
    , zoom : Zoom.Model
    , blockEvent : Maybe ( Block.Event, Block.Model )
    , zoomActivity : Maybe Activity.Model
    , route : Route
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        model =
            { activityCache = ActivityCache.initModel
            , zoom = Zoom.initModel Year (Date.fromCalendarDate 2018 Jan 1)
            , zoomActivity = Nothing
            , blockEvent = Nothing
            , route = parseLocation location
            }
    in
        case model.route of
            Route.NotFound ->
                model ! []

            Route.Blank ->
                model ! [ Task.perform (\date -> NewPage <| Route.Zoom <| Zoom.initModel Year date) Date.now ]

            Route.Zoom _ ->
                update (OnLocationChange location) model



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
                    Route.Zoom zoom ->
                        let
                            ( acModel, acMsg ) =
                                fetchActivities model.activityCache zoom.start zoom.end
                        in
                            { model | zoom = zoom, activityCache = acModel, route = newRoute }
                                ! [ acMsg |> Cmd.map UpdateActivityCache ]
                                |> filter (zoom.level == Week)
                                    (addCmd (Task.attempt (\r -> NoOp) (Dom.Scroll.toX "week-plot" 600)))

                    _ ->
                        ( { model | route = newRoute }, Cmd.none )

        NewPage page ->
            model ! [ Navigation.newUrl (Route.toString page) ]

        OpenActivity activity ->
            { model | zoomActivity = Just activity } ! []

        CloseActivity ->
            { model | zoomActivity = Nothing } ! []

        UpdateActivityCache subMsg ->
            { model | activityCache = ActivityCache.update subMsg model.activityCache } ! []

        NoOp ->
            model ! []

        BlockEvent event ->
            { model | blockEvent = event } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model.route of
        Route.Zoom zoom ->
            case zoom.level of
                Year ->
                    View.Zoom.year zoom model.blockEvent (accessActivities model.activityCache)

                Month ->
                    View.Zoom.month zoom model.blockEvent (accessActivities model.activityCache)

                Week ->
                    View.Zoom.week zoom model.blockEvent (accessActivities model.activityCache)

                _ ->
                    div [] [ Html.text "Invalid interval" ]

        Route.Blank ->
            div [] [ Html.text "Blank" ]

        Route.NotFound ->
            div [] [ Html.text "Not found" ]
