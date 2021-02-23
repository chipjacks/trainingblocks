module Store exposing (Model, cmd, flush, get, init, needsFlush, update)

import Activity exposing (Activity)
import Api
import Date exposing (Date)
import Effect exposing (Effect)
import Http
import Msg exposing (Msg(..))
import Process
import Set
import Task


type Model
    = Model State (List Msg) String


type alias State =
    { activities : List Activity
    , revision : String
    , level : Maybe Int
    }


init : String -> String -> List Activity -> Model
init csrfToken revision activities =
    Model (State activities revision Nothing |> updateLevel) [] csrfToken


get : Model -> (State -> b) -> b
get (Model state _ _) f =
    f state


cmd : Msg -> Effect
cmd msg =
    Effect.StoreCmd msg


needsFlush : Model -> Bool
needsFlush (Model _ msgs _) =
    not (List.isEmpty msgs)


updateState : Msg -> State -> State
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


updateLevel : State -> State
updateLevel state =
    let
        calculateLevel activities =
            activities
                |> List.filterMap Activity.mprLevel
                |> List.reverse
                |> List.head
    in
    { state | level = calculateLevel state.activities }


update : Msg -> Model -> ( Model, Effect )
update msg (Model state msgs csrfToken) =
    let
        model =
            Model state msgs csrfToken
    in
    case msg of
        Posted sentMsgs result ->
            case result of
                Ok ( rev, True ) ->
                    ( Model { state | revision = rev } msgs csrfToken
                    , Effect.None
                    )

                Err (Http.BadStatus 409) ->
                    ( Model state (msgs ++ sentMsgs) csrfToken
                    , Effect.Cmd (Task.attempt GotActivities Api.getActivities)
                    )

                _ ->
                    ( Model state (msgs ++ sentMsgs) csrfToken
                    , Effect.None
                    )

        DebounceFlush length ->
            if length == List.length msgs then
                ( Model state [] csrfToken
                , flush model
                )

            else
                ( model, Effect.None )

        GotActivities result ->
            case result of
                Ok ( revision, activities ) ->
                    let
                        newState =
                            List.foldr (\rmsg rs -> updateState rmsg rs) { state | activities = activities, revision = revision } msgs
                                |> updateLevel
                    in
                    ( Model newState msgs csrfToken
                    , debounceFlush (List.length msgs)
                    )

                Err _ ->
                    ( model, Effect.None )

        _ ->
            ( Model (updateState msg state) (msg :: msgs) csrfToken
            , debounceFlush (List.length msgs + 1)
            )


debounceFlush : Int -> Effect
debounceFlush length =
    Effect.Cmd (Task.perform (\_ -> DebounceFlush length) (Process.sleep 5000))


flush : Model -> Effect
flush model =
    case model of
        Model state [] _ ->
            Effect.None

        Model state msgs csrfToken ->
            Effect.PostActivities msgs
                { token = csrfToken
                , revision = state.revision
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


shiftUp : Activity.Id -> List Activity -> List Activity
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
