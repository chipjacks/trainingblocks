module BlockEvent exposing (State(..), Event(..), update, init)

import Mouse
import Block exposing (Data(..))
import Activity
import Date exposing (Date)
import Date.Extra as Date


type State
    = Viewing Mouse.Event Block.Model
    | Editing Mouse.Event Block.Model
    | Creating Mouse.Event Activity.Activity
    | None


type Event
    = View Mouse.Event Block.Model
    | EscapeView
    | Create Mouse.Event Date
    | Stretch Mouse.Event
    | EscapeCreate
    | Edit Mouse.Event Block.Model
    | Save Mouse.Event


init : State
init =
    None


update : Event -> State -> State
update event state =
    case event of
        View mouse block ->
            case state of
                None ->
                    Viewing mouse block

                _ ->
                    state

        Edit mouse block ->
            Editing mouse block

        Create mouse date ->
            let
                startDate =
                    Date.add Date.Minute ((Tuple.first mouse.offsetPos) / 912 * 24 * 60 |> round) date

                activity =
                    { id = Nothing
                    , name = "Run"
                    , distance = 6400
                    , duration = (30 * 60)
                    , type_ = Activity.Run
                    , startDate = startDate
                    , completed = False
                    , externalId = Nothing
                    }
            in
                Creating mouse activity

        Stretch mouse ->
            case state of
                Creating origMouse activity ->
                    let
                        minutes =
                            (((Tuple.first mouse.offsetPos) - (Tuple.first origMouse.offsetPos)) / 912 * 24 * 60) |> round

                        intensity =
                            (((Tuple.second mouse.offsetPos) - (Tuple.second origMouse.offsetPos)) / 10) |> round |> clamp 1 5

                        newActivity =
                            if minutes > 0 then
                                { activity | duration = minutes * 60 }
                            else
                                activity
                    in
                        Creating origMouse newActivity

                _ ->
                    state

        EscapeView ->
            case state of
                Viewing _ _ ->
                    None

                _ ->
                    state

        EscapeCreate ->
            case state of
                Creating mouse activity ->
                    Viewing mouse (Block.initModel (Activity activity))

                _ ->
                    state

        _ ->
            None
