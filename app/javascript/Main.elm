module Main exposing (Model, init, main, subscriptions, update, view, zoomToday)

import ActivityCache exposing (accessActivities, fetchActivities)
import Block
import Browser exposing (Document)
import Browser.Navigation as Navigation
import Date exposing (Date, Unit(..))
import Html exposing (Html, div)
import Msg exposing (Msg(..))
import Route exposing (Route, fromUrl)
import Task
import Time exposing (Month(..))
import Url exposing (Url)
import View.Block
import View.Zoom
import Zoom


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = ChangedUrl
        }



-- MODEL


type alias Model =
    { activityCache : ActivityCache.Model
    , zoom : Zoom.Model
    , route : Route
    , key : Navigation.Key
    }


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        model =
            { activityCache = ActivityCache.initModel
            , zoom = Zoom.initModel Years (Date.fromCalendarDate 2018 Jan 1)
            , route = fromUrl url
            , key = key
            }
    in
    case model.route of
        Route.NotFound ->
            zoomToday model

        Route.Zoom _ ->
            update (ChangedUrl url) model



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedUrl url ->
            let
                newRoute =
                    fromUrl url
            in
            case newRoute of
                Route.Zoom zoom ->
                    let
                        ( acModel, acMsg ) =
                            fetchActivities model.activityCache zoom.start zoom.end
                    in
                    ( { model | zoom = zoom, activityCache = acModel, route = newRoute }
                    , acMsg |> Cmd.map UpdateActivityCache
                    )

                Route.NotFound ->
                    zoomToday model

        ChangedRoute route ->
            ( model
            , Navigation.pushUrl model.key (Route.toString route)
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Navigation.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Navigation.load href
                    )

        ZoomToday ->
            zoomToday model

        UpdateActivityCache subMsg ->
            ( { model | activityCache = ActivityCache.update subMsg model.activityCache }
            , Cmd.none
            )

        NoOp ->
            ( model
            , Cmd.none
            )


zoomToday : Model -> ( Model, Cmd Msg )
zoomToday model =
    ( model
    , Task.perform (\date -> ChangedRoute (Route.Zoom <| Zoom.initModel model.zoom.level date)) Date.today
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Training Blocks"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    case model.route of
        Route.Zoom zoom ->
            div []
                [ View.Zoom.viewMenu model.zoom
                , View.Zoom.view zoom (accessActivities model.activityCache)
                ]

        Route.NotFound ->
            div [] [ Html.text "Not found" ]
