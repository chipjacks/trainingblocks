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
import Dom.Scroll
import Update.Extra exposing (filter, addCmd)
import Block
import View.Block
import Mouse exposing (Position)


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
    , route : Route
    , mousePos : Position
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        model =
            { activityCache = ActivityCache.initModel
            , zoom = Zoom.initModel Year (Date.fromCalendarDate 2018 Jan 1)
            , blockEvent = Nothing
            , route = parseLocation location
            , mousePos = Position 0 0
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

        UpdateActivityCache subMsg ->
            { model | activityCache = ActivityCache.update subMsg model.activityCache } ! []

        NoOp ->
            model ! []

        BlockEvent event ->
            { model | blockEvent = event } ! []

        MouseMsg position ->
            { model | mousePos = position } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves MouseMsg ]



-- VIEW


view : Model -> Html Msg
view model =
    case model.route of
        Route.Zoom zoom ->
            div []
                [ View.Zoom.view zoom (accessActivities model.activityCache)
                , View.Block.viewEvent model.mousePos model.blockEvent
                ]

        Route.Blank ->
            div [] [ Html.text "Blank" ]

        Route.NotFound ->
            div [] [ Html.text "Not found" ]
