module Msg exposing (ActivityConfigs, ActivityState(..), Msg(..), StoreData, Zoom(..))

import Activity.Types exposing (Activity)
import ActivityForm.Types exposing (ActivityForm)
import Browser.Dom as Dom
import Browser.Events as Events
import Date exposing (Date)
import Emoji exposing (EmojiDict)
import EmojiData exposing (EmojiData)
import Http
import Pace exposing (TrainingPace, TrainingPaceList)
import Store.History exposing (History)


type Zoom
    = Year
    | Month
    | Day


type ActivityState
    = Selected (List Activity)
    | Editing ActivityForm
    | Moving Activity Float Float
    | None


type alias ActivityConfigs =
    { paces : Maybe TrainingPaceList
    , emojis : EmojiDict
    }


type alias StoreData =
    { activities : List Activity
    , revision : String
    }


type Msg
    = LoadToday Date
    | GotActivities (Result Http.Error ( String, List Activity ))
    | FetchedEmojis (Result Http.Error (List EmojiData))
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
    | Undo ( Msg, StoreData )
    | Posted (History Msg StoreData) (Result Http.Error ( String, Bool ))
    | DebounceFlush Int
    | FlushNow
      -- CALENDAR
    | Jump Date
    | ChangeZoom Zoom (Maybe Date)
    | Scroll Bool Date Int
    | ScrollCompleted
    | ReceiveSelectDate String
    | MoveActivity Activity
      -- ACTIVITY FORM
    | ClickedNewActivity Date
    | NewActivity Activity
    | SelectActivity Activity Bool
    | EditActivity Activity
    | SelectedDate Date
    | EditedDescription String
    | SelectedLap Int
    | SelectedRepeatLap Int
    | ClickedAddLap
    | ClickedAddRepeat
    | SelectedEmoji String
    | SearchedEmojis String
    | CheckedCompleted
    | EditedRepeats String
    | EditedDistance String
    | SelectedDistanceUnits Activity.Types.DistanceUnits
    | SelectedActivityType Activity.Types.ActivityType
    | EditedDuration ( String, String, String )
    | SelectedEffort (Maybe Activity.Types.Effort)
    | SelectedPace String
    | CheckedRace
    | ClickedSubmit
    | ClickedEdit
    | ClickedCopy
    | ClickedRepeat
    | ClickedDelete
    | ClickedShift Bool
    | ClickedMove
    | ClickedGroup
    | ClickedClose
    | ClickedAutofill
    | ClickedClearLaps
    | NewId String
