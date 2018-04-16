module Main exposing (..)

import Html exposing (Html, div)
import Date exposing (Date, Month(..))
import Date.Extra as Date exposing (Interval(..))
import ActivityCache exposing (fetchActivities, accessActivities)
import Route exposing (Route, parseLocation)
import Navigation exposing (Location)
import Msg exposing (Msg(..))
import Task
import Zoom
import View.Zoom
import Block
import View.Block
import Mouse
import BlockEvent


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
    , blockEvent : BlockEvent.State
    , route : Route
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        model =
            { activityCache = ActivityCache.initModel
            , zoom = Zoom.initModel Year (Date.fromCalendarDate 2018 Jan 1)
            , blockEvent = BlockEvent.init
            , route = parseLocation location
            }
    in
        case model.route of
            Route.NotFound ->
                zoomToday model

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

                    Route.NotFound ->
                        zoomToday model

        ZoomToday ->
            zoomToday model

        NewPage page ->
            model ! [ Navigation.newUrl (Route.toString page) ]

        UpdateActivityCache subMsg ->
            { model | activityCache = ActivityCache.update subMsg model.activityCache } ! []

        NoOp ->
            model ! []

        UpdateBlockEvent event ->
            { model | blockEvent = BlockEvent.update event model.blockEvent } ! []


zoomToday : Model -> ( Model, Cmd Msg )
zoomToday model =
    model ! [ Task.perform (\date -> NewPage <| Route.Zoom <| Zoom.initModel model.zoom.level date) Date.now ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model.route of
        Route.Zoom zoom ->
            div []
                [ View.Zoom.viewMenu model.zoom
                , View.Zoom.view zoom (accessActivities model.activityCache)
                , BlockEvent.view model.blockEvent
                ]

        Route.NotFound ->
            div [] [ Html.text "Not found" ]
