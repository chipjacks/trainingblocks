module Msg exposing (Msg(..))

import ActivityCache
import Block
import Navigation exposing (Location)
import Route exposing (Route)
import Mouse
import DateTimePicker
import Date exposing (Date)


type Msg
    = UpdateActivityCache ActivityCache.Msg
    | OnLocationChange Location
    | NewPage Route
    | BlockEvent (Maybe ( Block.Event, Block.Model ))
    | MouseMsg Mouse.Position
    | DateChange DateTimePicker.State (Maybe Date)
    | NoOp
