module Msg exposing (Msg(..))

import ActivityCache
import Block
import Mouse
import Url exposing (Url)
import Route exposing (Route)


type Msg
    = UpdateActivityCache ActivityCache.Msg
    | ChangedUrl Url
    | NewPage Route
    | BlockEvent (Maybe ( Block.Event, Block.Model ))
    | MouseMsg Mouse.Position
    | ZoomToday
    | NoOp
