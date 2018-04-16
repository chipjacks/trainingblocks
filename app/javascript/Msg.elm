module Msg exposing (Msg(..))

import ActivityCache
import Block
import Navigation exposing (Location)
import Route exposing (Route)
import Mouse
import BlockEvent


type Msg
    = UpdateActivityCache ActivityCache.Msg
    | OnLocationChange Location
    | NewPage Route
    | UpdateBlockEvent BlockEvent.Event
    | ZoomToday
    | NoOp
