module ActivityCache exposing (Model, Msg, accessActivities, fetchActivities, initModel, update)

import Activity exposing (Activity)
import Date exposing (Date, Interval(..), toRataDie)
import Dict exposing (Dict)
import RemoteData exposing (RemoteData(..), WebData)


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
                            filterActivities date (Date.add Date.Months 1 date) allActivities
                    in
                    { model | cache = Dict.insert (date |> keyFor) (Success activities) model.cache }

                _ ->
                    Debug.todo "failure!"


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
    List.filter (\activity -> Date.isBetween a (Date.add Date.Days -1 b) activity.startDate)


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
            ( { model | cache = Dict.insert key Loading model.cache }, stravaRequestCmd date :: cmds )


stravaRequestCmd : Date -> Cmd Msg
stravaRequestCmd date =
    let
        endDate =
            Date.add Date.Months 1 date
    in
    Activity.list date endDate
        |> RemoteData.sendRequest
        |> Cmd.map (GotActivities date)
