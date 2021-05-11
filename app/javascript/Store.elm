module Store exposing (Model, cmd, get, init, needsFlush, update)

import Activity
import Activity.Types exposing (Activity, Id)
import Api
import Date exposing (Date)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Emoji
import EmojiData exposing (EmojiData)
import Http
import Msg exposing (ActivityConfigs, Msg(..), StoreData)
import Process
import Set
import Store.History as History exposing (History)
import Task


type Model
    = Model StoreData (History Msg StoreData)


init : String -> List Activity -> Model
init revision activities =
    let
        configs =
            { levelM = Nothing
            , emojis = Dict.empty
            }
    in
    Model (StoreData activities revision configs |> updateLevel) History.init


get : Model -> (StoreData -> b) -> b
get (Model state _) f =
    f state


cmd : Msg -> Effect
cmd msg =
    Effect.StoreCmd msg


needsFlush : Model -> Bool
needsFlush (Model _ history) =
    not (History.isEmpty history)


updateState : Msg -> StoreData -> StoreData
updateState msg state =
    case msg of
        Create activity ->
            { state | activities = updateActivity activity True state.activities }
                |> updateLevel

        Update activity ->
            { state | activities = updateActivity activity False state.activities }
                |> updateLevel

        Delete activity ->
            { state | activities = List.filter (\a -> a.id /= activity.id) state.activities }
                |> updateLevel

        Group activities session ->
            let
                ids =
                    List.map .id activities

                removeActivities =
                    List.filter (\a -> not (List.member a.id ids)) state.activities
            in
            { state | activities = updateActivity session True removeActivities }

        Ungroup activities session ->
            let
                ungrouped =
                    List.foldl (\a b -> updateActivity { a | date = session.date } True b) state.activities activities
            in
            { state | activities = List.filter (\a -> a.id /= session.id) ungrouped }

        Move date activity ->
            { state | activities = moveActivity activity date state.activities }

        Shift up activity ->
            { state | activities = shiftActivity activity up state.activities }

        _ ->
            state


updateLevel : StoreData -> StoreData
updateLevel state =
    let
        calculateLevel activities =
            activities
                |> List.filterMap Activity.mprLevel
                |> List.reverse
                |> List.head
    in
    updateConfigs (\c -> { c | levelM = calculateLevel state.activities }) state


updateConfigs : (ActivityConfigs -> ActivityConfigs) -> StoreData -> StoreData
updateConfigs transform state =
    { state | configs = transform state.configs }


update : Msg -> Model -> ( Model, Effect )
update msg (Model state history) =
    let
        model =
            Model state history
    in
    case msg of
        Posted sentHistory result ->
            case result of
                Ok ( rev, True ) ->
                    ( Model { state | revision = rev } history
                    , Effect.None
                    )

                Err (Http.BadStatus 409) ->
                    ( Model state (History.append history sentHistory)
                    , Effect.Cmd (Task.attempt GotActivities Api.getActivities)
                    )

                _ ->
                    ( Model state (History.append history sentHistory)
                    , Effect.None
                    )

        FlushNow ->
            ( Model state History.init
            , flush model
            )

        DebounceFlush length ->
            if length == History.length history then
                ( Model state History.init
                , flush model
                )

            else
                ( model, Effect.None )

        GotActivities result ->
            case result of
                Ok ( revision, activities ) ->
                    let
                        newState =
                            List.foldr (\rmsg rs -> updateState rmsg rs) { state | activities = activities, revision = revision } (History.events history)
                                |> updateLevel
                    in
                    ( Model newState history
                    , debounceFlush (History.length history)
                    )

                Err _ ->
                    ( model, Effect.None )

        FetchedEmojis result ->
            case result of
                Ok emojis ->
                    ( Model (updateConfigs (\c -> { c | emojis = Emoji.toDict emojis }) state) history
                    , Effect.None
                    )

                _ ->
                    ( model, Effect.None )

        _ ->
            ( Model (updateState msg state) (History.push ( msg, state ) history)
            , debounceFlush (History.length history + 1)
            )


debounceFlush : Int -> Effect
debounceFlush length =
    Effect.Cmd (Task.perform (\_ -> DebounceFlush length) (Process.sleep 5000))


flush : Model -> Effect
flush (Model state history) =
    if History.isEmpty history then
        Effect.None

    else
        let
            msgs =
                History.events history
        in
        Effect.PostActivities history
            { revision = state.revision
            , orderUpdates = orderUpdates state.activities msgs
            , activityUpdates = activityUpdates msgs
            }


updateActivity : Activity -> Bool -> List Activity -> List Activity
updateActivity activity isNew activities =
    if isNew then
        List.partition (\a -> Date.compare a.date activity.date == GT) activities
            |> (\( after, before ) -> List.concat [ before, [ activity ], after ])

    else
        List.map
            (\existing ->
                if existing.id == activity.id then
                    activity

                else
                    existing
            )
            activities


moveActivity : Activity -> Date -> List Activity -> List Activity
moveActivity activity toDate activities =
    updateActivity { activity | date = toDate } True (List.filter (\a -> a.id /= activity.id) activities)


shiftActivity : Activity -> Bool -> List Activity -> List Activity
shiftActivity activity moveUp activities =
    let
        before =
            List.filter (\a -> Date.compare a.date activity.date == LT) activities

        on =
            List.filter (\a -> a.date == activity.date) activities

        after =
            List.filter (\a -> Date.compare a.date activity.date == GT) activities
    in
    if moveUp then
        List.concat [ before, shiftUp activity.id on, after ]

    else
        List.concat [ before, shiftUp activity.id (List.reverse on) |> List.reverse, after ]


shiftUp : Id -> List Activity -> List Activity
shiftUp id activities =
    case activities of
        a :: b :: tail ->
            if a.id == id then
                activities

            else if b.id == id then
                b :: a :: tail

            else
                a :: shiftUp id (b :: tail)

        _ ->
            activities


activityUpdates : List Msg -> List ( String, Activity )
activityUpdates msgs =
    let
        activityChange m =
            case m of
                Create a ->
                    [ ( "create", a ) ]

                Move date a ->
                    [ ( "update", { a | date = date } ) ]

                Update a ->
                    [ ( "update", a ) ]

                Delete a ->
                    [ ( "delete", a ) ]

                Group activities session ->
                    ( "create", session )
                        :: List.map (\a -> ( "delete", a )) activities

                Ungroup activities session ->
                    ( "delete", session )
                        :: List.map (\a -> ( "create", a )) activities

                _ ->
                    []
    in
    List.reverse msgs
        |> List.map activityChange
        |> List.concat


orderUpdates : List Activity -> List Msg -> List ( String, Int )
orderUpdates activities msgs =
    let
        orderingChange m =
            case m of
                Create a ->
                    Just a.date

                Delete a ->
                    Just a.date

                Group _ session ->
                    Just session.date

                Ungroup _ session ->
                    Just session.date

                Move date a ->
                    Just date

                Shift _ a ->
                    Just a.date

                _ ->
                    Nothing

        dates =
            msgs
                |> List.filterMap orderingChange
                |> List.map Date.toRataDie
                |> Set.fromList
                |> Set.toList
                |> List.sort
                |> List.map Date.fromRataDie
    in
    dates
        |> List.map (\d -> List.filter (\a -> a.date == d) activities)
        |> List.map (\l -> List.indexedMap (\i a -> ( a.id, i )) l)
        |> List.concat
