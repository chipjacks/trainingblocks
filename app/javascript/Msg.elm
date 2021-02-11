module Msg exposing (ActivityForm, ActivityState(..), FormError(..), Msg(..), Zoom(..))

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
    , activityType : Activity.ActivityType
    , duration : String
    , completed : Bool
    , pace : String
    , distance : Maybe Activity.Distance
    , effort : Maybe Activity.Effort
    , emoji : String
    }


type FormError
    = ApiError
    | EmptyFieldError String


type ActivityState
    = Selected (List Activity)
    | Editing ActivityForm
    | Moving Activity Float Float
    | None


type Msg
    = LoadToday Date
    | GotActivities (Result Http.Error ( String, List Activity ))
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
    | Posted (List Msg) (Result Http.Error ( String, Bool ))
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
    | EditedDescription String
    | SelectedEmoji String
    | CheckedCompleted
    | SelectedActivityType Activity.ActivityType
    | EditedDuration String
    | SelectedEffort (Maybe Activity.Effort)
    | SelectedPace String
    | SelectedDistance Activity.Distance
    | ClickedSubmit
    | ClickedCopy Activity
    | ClickedMove
    | ClickedGroup
    | ClickedUngroup Activity
    | ClickedClose
    | NewId String
