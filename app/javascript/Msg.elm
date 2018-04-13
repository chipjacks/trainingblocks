module Msg exposing (Msg(..))

import ActivityCache
import Block
import Navigation exposing (Location)
import Route exposing (Route)
import SvgMouseEvents exposing (MouseEvent)


type Msg
    = UpdateActivityCache ActivityCache.Msg
    | OnLocationChange Location
    | NewPage Route
    | BlockEvent (Maybe ( MouseEvent, Block.Event, Block.Model ))
    | ZoomToday
    | NoOp
