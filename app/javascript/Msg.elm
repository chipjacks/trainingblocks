module Msg exposing (Msg(..))

import Activity
import ActivityCache
import Block
import Navigation exposing (Location)
import Route exposing (Route)


type Msg
    = OpenActivity Activity.Model
    | CloseActivity
    | UpdateActivityCache ActivityCache.Msg
    | OnLocationChange Location
    | NewPage Route
    | BlockEvent (Maybe ( Block.Event, Block.Model ))
    | NoOp
