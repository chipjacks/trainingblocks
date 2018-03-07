module Container exposing (..)

import RemoteData exposing (WebData, RemoteData(..))
import Date exposing (Date)
import Date.Extra as Date exposing (Interval(..), isBetween)
import StravaAPI exposing (StravaAPIActivity)
import Activity exposing (fromStravaAPIActivity)
import Html exposing (Html, div, span)
import Html.Attributes exposing (style)
import Updater exposing (Updater, Converter, converter, noReaction, toCmd)
import Updater.Many as Many
import Week


type alias WeeksModel =
    Many.Model Week.Model Week.Msg


type alias WeeksMsg =
    Many.Msg Week.Model Week.Msg


type alias Model =
    { -- id: BlockId
      --   , userId: UserId,
      activities : WebData (List Activity.Model)
    , weeks : WeeksModel
    , scale: Date.Interval
    , date : Date
    -- Maybe add some aggregated metrics here so recursive load can happen lazily
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



init : Date.Interval -> Date -> ( Model, Cmd Msg )
init scale date =
    let
        model =
            { activities = NotAsked, weeks = Many.initModel Week.update Week.subscriptions, scale = scale, date = date }
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
    Date.add model.scale 1 model.date


-- UPDATE


type Msg
    = GotStravaActivities (WebData (List StravaAPIActivity))
    | FetchActivities
    | UpdaterMsg (Updater Model Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdaterMsg u ->
                u model

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
                        ( { model | activities = Success activities }, updateWeeksActivities activities model)

                Failure msg ->
                    ( model, Cmd.none )

                Loading ->
                    ( { model | activities = Loading }, Cmd.none )

                NotAsked ->
                    ( { model | activities = NotAsked }, Cmd.none )


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
    div [ style [ ( "height", "100px" ) ] ]
        ((span [] [ Html.text ((Date.toFormattedString "MMM d" model.date) ++ " - " ++ (Date.toFormattedString "MMM d" <| endDate model)) ])
            :: [(Html.map weeksC <| viewWeeks model.weeks)]
        )

viewWeeks : WeeksModel -> Html WeeksMsg
viewWeeks weeks = 
    div [ ]
        [ div [ style [] ] <|
              weeks.viewAll
              (\ id week conv -> Just <| conv <| Week.view week)
        ]
