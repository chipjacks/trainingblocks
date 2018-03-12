module ActivityCache exposing (fetchActivities, accessActivities, Model, initModel, Msg, update)

import Dict exposing (Dict)
import Date exposing (Date)
import Date.Extra as Date exposing (toRataDie, Interval(..))
import RemoteData exposing (WebData, RemoteData(..))
import StravaAPI exposing (StravaAPIActivity)
import Activity


type alias Model =
    { cache : Dict Int (WebData (List Activity.Model))
    }


initModel : Model
initModel =
    Model Dict.empty


type Msg
    = GotStravaActivities Date (WebData (List StravaAPIActivity))


update : Msg -> Model -> Model
update msg model =
    case msg of
        GotStravaActivities date webdata ->
            case webdata of
                Success stravaActivities ->
                    let
                        activities =
                            List.map Activity.fromStravaAPIActivity stravaActivities
                                -- TODO: the API should do this filtering
                                |> List.filter (\a -> Date.isBetween date (Date.add Month 1 date) a.date)
                    in
                        { model | cache = Dict.insert (date |> keyFor) (Success activities) model.cache }

                _ ->
                    model


fetchActivities : Model -> ( Date, Date ) -> ( Model, Cmd Msg )
fetchActivities model ( startDate, endDate ) =
    Date.range Date.Month 1 startDate endDate
        |> List.foldr fetchIfMissing ( model, [] )
        |> (\( m, cs ) -> ( m, Cmd.batch cs ))


accessActivities : Model -> Date -> Date -> WebData (List Activity.Model)
accessActivities model startDate endDate =
    RemoteData.map2
        (\a b -> a ++ b)
        (Dict.get (startDate |> keyFor) model.cache |> Maybe.withDefault NotAsked)
        (Dict.get (endDate |> keyFor) model.cache |> Maybe.withDefault NotAsked)
        |> RemoteData.map (\l -> List.filter (\a -> Date.isBetween startDate endDate a.date) l)



--- INTERNAL


keyFor : Date -> Int
keyFor date =
    Date.floor Date.Month date |> toRataDie


fetchIfMissing : Date -> ( Model, List (Cmd Msg) ) -> ( Model, List (Cmd Msg) )
fetchIfMissing date result =
    let
        key =
            keyFor date

        value =
            Dict.get key model.cache

        ( model, cmds ) =
            result
    in
        case value of
            Just webdata ->
                result

            Nothing ->
                ( { model | cache = Dict.insert key Loading model.cache }, (stravaRequestCmd date) :: cmds )


stravaRequestCmd : Date -> Cmd Msg
stravaRequestCmd date =
    let
        endDate =
            Date.add Month 1 date
    in
        StravaAPI.listActivities date endDate
            |> RemoteData.sendRequest
            |> Cmd.map (GotStravaActivities date)
