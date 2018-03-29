module ActivityCache exposing (fetchActivities, accessActivities, Model, initModel, Msg, update)

import Dict exposing (Dict)
import Date exposing (Date)
import Date.Extra as Date exposing (toRataDie, Interval(..))
import RemoteData exposing (WebData, RemoteData(..))
import Activity exposing (Activity)


type alias Model =
    { cache : Dict Int (WebData (List Activity))
    }


initModel : Model
initModel =
    Model Dict.empty


type Msg
    = GotActivities Date (WebData (List Activity))


update : Msg -> Model -> Model
update msg model =
    case msg of
        GotActivities date webdata ->
            case webdata of
                Success allActivities ->
                    let
                        activities =
                            filterActivities date (Date.add Month 1 date) allActivities
                    in
                        { model | cache = Dict.insert (date |> keyFor) (Success activities) model.cache }

                _ ->
                    model


fetchActivities : Model -> Date -> Date -> ( Model, Cmd Msg )
fetchActivities model startDate endDate =
    Date.range Date.Month 1 (Date.floor Date.Month startDate) endDate
        |> List.foldr fetchIfMissing ( model, [] )
        |> (\( m, cs ) -> ( m, Cmd.batch cs ))


accessActivities : Model -> Date -> Date -> WebData (List Activity)
accessActivities model startDate endDate =
    Date.range Date.Month 1 (Date.floor Date.Month startDate) endDate
        |> List.map keyFor
        |> List.map (\k -> Dict.get k model.cache |> Maybe.withDefault NotAsked)
        |> List.foldl (\r s -> RemoteData.map2 (++) s r) (RemoteData.succeed [])
        |> RemoteData.map (filterActivities startDate endDate)



--- INTERNAL


keyFor : Date -> Int
keyFor date =
    Date.floor Date.Month date |> toRataDie


filterActivities : Date -> Date -> (List Activity -> List Activity)
filterActivities a b =
    List.filter (\activity -> Date.isBetween a (Date.add Date.Second -1 b) activity.startDate)


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
        Activity.list date endDate
            |> RemoteData.sendRequest
            |> Cmd.map (GotActivities date)
