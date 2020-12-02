module Msg exposing (ActivityForm, ActivityState(..), DataForm(..), FormError(..), Msg(..), Zoom(..))

import Activity exposing (Activity)
import Browser.Dom as Dom
import Browser.Events as Events
import Date exposing (Date)
import Http


type Zoom
    = Year
    | Month
    | Day


type alias ActivityForm =
    { id : Activity.Id
    , date : Maybe Date
    , description : String
    , result : Result FormError Activity
    , dataForm : DataForm
    }


type FormError
    = ApiError
    | EmptyFieldError String


type DataForm
    = RunForm { duration : String, pace : Activity.Pace, completed : Bool }
    | IntervalForm { duration : String, pace : Activity.Pace, completed : Bool }
    | RaceForm { duration : String, distance : Activity.Distance, completed : Bool }
    | OtherForm { duration : String, completed : Bool }
    | NoteForm { emoji : String }
    | SessionForm (List Activity)


type ActivityState
    = Selected (List Activity)
    | Editing ActivityForm
    | Moving Activity Float Float
    | None


type Msg
    = LoadToday Date
    | GotActivities (Result String (List Activity))
    | VisibilityChange Events.Visibility
    | KeyPressed String
    | MouseMoved Float Float
    | AutoScrollCalendar Float
    | MouseReleased
    | MoveTo Date
    | NoOp
      -- STORE
    | Create Activity
    | Group (List Activity) Activity
    | Ungroup (List Activity) Activity
    | Update Activity
    | Move Date Activity
    | Shift Bool Activity
    | Delete Activity
    | Posted (List Msg) (Result String Bool)
    | DebounceFlush Int
      -- CALENDAR
    | Jump Date
    | ChangeZoom Zoom (Maybe Date)
    | Scroll Bool Date Int
    | ScrollCompleted (Result Dom.Error Dom.Element)
    | ReceiveSelectDate String
    | MoveActivity Activity
      -- ACTIVITY FORM
    | ClickedNewActivity Date
    | NewActivity Activity
    | SelectActivity Activity Bool
    | EditActivity Activity
    | SelectedDate Date
    | SelectedShape Activity.ActivityData
    | EditedDescription String
    | SelectedEmoji String
    | CheckedCompleted
    | EditedDuration String
    | SelectedPace String
    | SelectedDistance String
    | ClickedSubmit
    | ClickedCopy Activity
    | ClickedMove Activity
    | ClickedGroup
    | ClickedClose
    | NewId String
