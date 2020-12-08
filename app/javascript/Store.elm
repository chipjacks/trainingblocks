module Store exposing (Model, cmd, flush, get, init, needsFlush, update)

import Activity exposing (Activity)
import Api
import Date exposing (Date)
import Http
import Msg exposing (Msg(..))
import Process
import Task exposing (Task)


type Model
    = Model State (List Msg) String


type alias State =
    { activities : List Activity }


init : String -> List Activity -> Model
init csrfToken activities =
    Model (State activities) [] csrfToken


get : Model -> (State -> b) -> b
get (Model state _ _) f =
    f state


cmd : Msg -> Cmd Msg
cmd msg =
    Task.perform (\_ -> msg) (Task.succeed ())


needsFlush : Model -> Bool
needsFlush (Model _ msgs _) =
    not (List.isEmpty msgs)


updateState : Msg -> State -> State
updateState msg state =
    case msg of
        Create activity ->
            { state | activities = updateActivity activity True state.activities }

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

        Update activity ->
            { state | activities = updateActivity activity False state.activities }

        Move date activity ->
            { state | activities = moveActivity activity date state.activities }

        Shift up activity ->
            { state | activities = shiftActivity activity up state.activities }

        Delete activity ->
            { state | activities = List.filter (\a -> a.id /= activity.id) state.activities }

        _ ->
            state


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model state msgs csrfToken) =
    let
        model =
            Model state msgs csrfToken
    in
    case msg of
        Posted sentMsgs result ->
            case result of
                Ok True ->
                    ( Model state msgs csrfToken
                    , Cmd.none
                    )

                _ ->
                    ( Model state (msgs ++ sentMsgs) csrfToken
                    , Cmd.none
                    )

        DebounceFlush length ->
            if length == List.length msgs then
                ( Model state [] csrfToken
                , flush model
                )

            else
                ( model, Cmd.none )

        GotActivities result ->
            case result of
                Ok activities ->
                    let
                        newState =
                            List.foldr (\rmsg rs -> updateState rmsg rs) (State activities) msgs
                    in
                    ( Model newState msgs csrfToken, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        _ ->
            ( Model (updateState msg state) (msg :: msgs) csrfToken
            , debounceFlush (List.length msgs + 1)
            )


debounceFlush : Int -> Cmd Msg
debounceFlush length =
    Task.perform (\_ -> DebounceFlush length) (Process.sleep 5000)


flush : Model -> Cmd Msg
flush model =
    case model of
        Model state [] _ ->
            Cmd.none

        Model state msgs csrfToken ->
            Api.getActivities
                |> Task.map State
                |> Task.map (\remoteState -> List.foldr (\msg rs -> updateState msg rs) remoteState msgs)
                |> Task.andThen (\newRemoteState -> Api.postActivities csrfToken newRemoteState.activities)
                |> Task.attempt (Posted msgs)


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
