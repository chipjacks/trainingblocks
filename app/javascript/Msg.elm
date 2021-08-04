module Msg exposing (ActivityConfigs, ActivityState(..), Msg(..), StoreData, Zoom(..))

import Activity.Types exposing (Activity)
import ActivityForm.Types exposing (ActivityForm)
import Browser.Events as Events
import Date exposing (Date)
import Emoji exposing (EmojiDict)
import EmojiData exposing (EmojiData)
import Http
import Pace exposing (StandardPace)
import Pace.List exposing (PaceList)
import Settings exposing (Settings)
import Store.History exposing (History)
import Uuid


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
    { paces : PaceList StandardPace
    , customPaces : PaceList String
    , emojis : EmojiDict
    }


type alias StoreData =
    { activities : List Activity
    , revision : String
    }


type Msg
    = LoadToday Date
    | GotActivities (Result Http.Error ( String, List Activity ))
    | GotSettings (Result Http.Error (Maybe Settings))
    | FetchedEmojis (Result Http.Error (List EmojiData))
    | VisibilityChange Events.Visibility
    | KeyPressed String
    | MouseMoved Float Float
    | AutoScrollCalendar Float
    | MouseReleased
    | MoveTo Date
    | ReportedError (Result Http.Error Uuid.Uuid)
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
    | Scroll Bool
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
    | EditedPace String
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
