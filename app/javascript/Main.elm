module Main exposing (..)

import Html exposing (Html, div, button, i)
import Html.Attributes exposing (class)
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
import OnClickPage exposing (onClickPage)
import DateTimePicker


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
    , datePickerState : DateTimePicker.State
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
            , datePickerState = DateTimePicker.initialState
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

        DateChange datePickerState selectedDate ->
            let
                zoom =
                    model.zoom

                newDate =
                    selectedDate |> Maybe.withDefault model.zoom.end

                newUrl =
                    (Route.toString <| Route.Zoom <| Zoom.initModel model.zoom.level newDate)
            in
                { model | datePickerState = datePickerState }
                    ! [ Navigation.newUrl newUrl ]



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
                [ viewHeader model
                , View.Zoom.view zoom (accessActivities model.activityCache)
                , View.Block.viewEvent model.mousePos model.blockEvent
                ]

        Route.Blank ->
            div [] [ Html.text "Blank" ]

        Route.NotFound ->
            div [] [ Html.text "Not found" ]


viewHeader : Model -> Html Msg
viewHeader model =
    let
        zoom =
            model.zoom
    in
        div [ class "ui secondary menu" ]
            [ div [ class "ui simple dropdown item" ]
                [ Html.text (toString model.zoom.level)
                , Html.i [ class "dropdown icon" ] []
                , div [ class "menu" ]
                    [ div ([ class "item" ] ++ (onClickPage (Route.Zoom { zoom | level = Year }))) [ Html.text "Year" ]
                    , div ([ class "item" ] ++ (onClickPage (Route.Zoom { zoom | level = Month }))) [ Html.text "Month" ]
                    , div ([ class "item" ] ++ (onClickPage (Route.Zoom { zoom | level = Week }))) [ Html.text "Week" ]
                    ]
                ]
            , div [ class "ui input item" ]
                [ DateTimePicker.datePicker
                    DateChange
                    [ class "ui input" ]
                    model.datePickerState
                    (Just model.zoom.end)
                ]
            , div [ class "ui item" ]
                [ button ([ class "ui left attached basic icon button" ] ++ (onClickPage (Route.Zoom (Zoom.older zoom))))
                    [ Html.i [ class "arrow left icon" ] []
                    ]
                , button ([ class "ui right attached basic icon button" ] ++ (onClickPage (Route.Zoom (Zoom.newer zoom))))
                    [ Html.i [ class "arrow right icon" ] []
                    ]
                ]
            ]
