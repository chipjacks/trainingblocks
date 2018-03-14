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
import View


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
    case model.route of
        Route.Zoom zoom ->
            case zoom.level of
                Year ->
                    View.year zoom (accessActivities model.activityCache)

                Month ->
                    View.month zoom (accessActivities model.activityCache)

                Week ->
                    View.week zoom (accessActivities model.activityCache)

                _ ->
                    div [] [ Html.text "Invalid interval" ]

        Route.Blank ->
            div [] [ Html.text "Blank" ]

        Route.NotFound ->
            div [] [ Html.text "Not found" ]
