module Msg exposing (Msg(..))

import ActivityCache
import Block
import Navigation exposing (Location)
import Route exposing (Route)
import Mouse


type Msg
    = UpdateActivityCache ActivityCache.Msg
    | OnLocationChange Location
    | NewPage Route
    | BlockEvent (Maybe ( Block.Event, Block.Model ))
    | MouseMsg Mouse.Position
    | ZoomToday
    | NoOp
