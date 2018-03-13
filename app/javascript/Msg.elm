module Msg exposing (Msg(..))

import Activity
import ActivityCache
import Navigation exposing (Location)
import Route exposing (Route)

type Msg
    = OpenActivity Activity.Model
    | CloseActivity
    | UpdateActivityCache ActivityCache.Msg
    | OnLocationChange Location
    | NewPage Route