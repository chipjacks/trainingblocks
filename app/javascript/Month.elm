module Month exposing (..)

import RemoteData exposing (WebData, RemoteData(..))
import Date exposing (Date)
import Date.Extra as Date exposing (Interval(..), isBetween)
import StravaAPI exposing (StravaAPIActivity)
import Activity exposing (fromStravaAPIActivity)
import Html exposing (Html, div, span)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick)
import Updater exposing (Updater, Converter, converter, noReaction, toCmd)
import Updater.Many as Many
import Week


type alias WeeksModel =
    Many.Model Week.Model Week.Msg


type alias WeeksMsg =
    Many.Msg Week.Model Week.Msg


type alias Model =
    { status : RemoteData String ()
    , activities : List Activity.Model
    , weeks : WeeksModel
    , date : Date
    , expanded : Bool
    }


weeksC : Converter Msg WeeksMsg
weeksC =
    converter
        UpdaterMsg
        { get = Just << .weeks
        , set = (\cm model -> { model | weeks = cm })
        , update = Many.update
        , react = noReaction
        }


init : Date -> ( Model, Cmd Msg )
init date =
    let
        model =
            { status = NotAsked, activities = [], weeks = Many.initModel Week.update Week.subscriptions, date = date, expanded = False }
    in
        ( model, loadActivities model )


loadActivities : Model -> Cmd Msg
loadActivities model =
    let
        startDate =
            model.date
    in
        StravaAPI.listActivities startDate (endDate model)
            |> RemoteData.sendRequest
            |> Cmd.map GotStravaActivities


endDate : Model -> Date
endDate model =
    Date.add Month 1 model.date


-- UPDATE


type Msg
    = GotStravaActivities (WebData (List StravaAPIActivity))
    | FetchActivities
    | ToggleExpanded
    | UpdaterMsg (Updater Model Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdaterMsg u ->
                u model

        ToggleExpanded ->
            ({model | expanded = not model.expanded}, Cmd.none)

        FetchActivities ->
            ( model, (loadActivities model) )

        GotStravaActivities webdata ->
            case webdata of
                Success stravaActivities ->
                    let
                        activities =
                            List.map fromStravaAPIActivity stravaActivities
                              -- TODO: the API should do this filtering
                              |> List.filter (\a -> isBetween model.date (endDate model) a.date)
                    in
                        ( { model | activities = activities, status = Success () }, updateWeeksActivities activities model)

                Failure msg ->
                    ( model, Cmd.none )

                Loading ->
                    ( { model | status = Loading }, Cmd.none )

                NotAsked ->
                    ( { model | status = NotAsked }, Cmd.none )


updateWeeksActivities : List Activity.Model -> Model -> Cmd Msg
updateWeeksActivities activities model =
    let
        minWeek = Date.floor Date.Week model.date
        maxWeek = Date.add Date.Month 1 minWeek
    in
        Date.range Date.Week 1 minWeek maxWeek
            |> List.map (\d -> toCmd <| weeksC <| Many.Add <| Week.init activities d)
            |> Cmd.batch


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model = 
    div [ cssClass model, onClick ToggleExpanded ]
        [ span [] [ Html.text (Date.toFormattedString "MMMM" model.date) ]
        , viewActivities model
        ]


viewActivities : Model -> Html Msg
viewActivities model =
    case model.expanded of
        True ->
            model.weeks.viewAll (\ id week conv -> Week.viewCompact week |> conv |> Just) |> div [ class "weeks" ] |> Html.map weeksC
        False ->
            Activity.viewTreemap model.activities


cssClass : Model -> Html.Attribute Msg
cssClass model =
    case model.expanded of
        True ->
            class "month open"
    
        False ->
            class "month"