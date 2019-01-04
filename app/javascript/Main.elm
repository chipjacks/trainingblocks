module Main exposing (Model, init, main, subscriptions, update, view, zoomToday)

import ActivityCache exposing (accessActivities, fetchActivities)
import Block
import Date exposing (Date, Month(..))
import Html exposing (Html, div)
import Mouse exposing (Position)
import Msg exposing (Msg(..))
import Url exposing (Url)
import Route exposing (Route, fromUrl)
import Task
import View.Block
import View.Zoom
import Zoom


main : Program Never Model Msg
main =
    Navigation.program ChangedUrl
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


init : Url -> ( Model, Cmd Msg )
init url =
    let
        model =
            { activityCache = ActivityCache.initModel
            , zoom = Zoom.initModel Year (Date.fromCalendarDate 2018 Jan 1)
            , blockEvent = Nothing
            , route = fromUrl url
            , mousePos = Position 0 0
            }
    in
    case model.route of
        Route.NotFound ->
            zoomToday model

        Route.Zoom _ ->
            update (UrlChanged url) model



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

        ZoomToday ->
            zoomToday model

        NewPage page ->
            ( model
            , Navigation.newUrl (Route.toString page)
            )

        UpdateActivityCache subMsg ->
            ( { model | activityCache = ActivityCache.update subMsg model.activityCache }
            , Cmd.none
            )

        NoOp ->
            ( model
            , Cmd.none
            )

        BlockEvent event ->
            ( { model | blockEvent = event }
            , Cmd.none
            )

        MouseMsg position ->
            ( { model | mousePos = position }
            , Cmd.none
            )


zoomToday : Model -> ( Model, Cmd Msg )
zoomToday model =
    ( model
    , Task.perform (\date -> NewPage <| Route.Zoom <| Zoom.initModel model.zoom.level date) Date.now
    )



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
                [ View.Zoom.viewMenu model.zoom
                , View.Zoom.view zoom (accessActivities model.activityCache)
                , View.Block.viewEvent model.mousePos model.blockEvent
                ]

        Route.NotFound ->
            div [] [ Html.text "Not found" ]
