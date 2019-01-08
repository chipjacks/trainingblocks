module Msg exposing (Msg(..))

import ActivityCache
import Block
import Browser
import Route exposing (Route)
import Url exposing (Url)


type Msg
    = UpdateActivityCache ActivityCache.Msg
    | ChangedUrl Url
    | LinkClicked Browser.UrlRequest
    | ChangedRoute Route
    | ZoomToday
    | NoOp
