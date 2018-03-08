module Week exposing (..)

import Updater exposing (Updater, Converter, converter, noReaction, toCmd)
import Updater.Many as Many
import Activity exposing (ActivityType, activityTypes)
import Date exposing (Date)
import Date.Extra as Date exposing (toFormattedString, Interval(..), isBetween)
import Html exposing (Html, div, span, text, button)
import Html.Attributes exposing (class, style)
import Dict
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (width, height, x, y)
import Treemap exposing (Coordinate, Container)

type alias ActivitiesModel =
    Many.Model Activity.Model Activity.Msg


type alias ActivitiesMsg =
    Many.Msg Activity.Model Activity.Msg


type alias Model =
    {
        activities : ActivitiesModel
    ,   date : Date
    }

type Msg
    = UpdaterMsg (Updater Model Msg)

activitiesC : Converter Msg ActivitiesMsg
activitiesC =
    converter
        UpdaterMsg
        { get = Just << .activities
        , set = (\cm model -> { model | activities = cm })
        , update = Many.update
        , react = noReaction
        }


init : List Activity.Model -> Date ->  ( Model, Cmd Msg )
init activities date =
    let
        model = { activities = Many.initModel Activity.update Activity.subscriptions , date = date }
    in
        model ! (loadActivities activities model)


loadActivities : List Activity.Model -> Model -> List (Cmd Msg)
loadActivities activities model = 
    List.filter (\a -> isBetween model.date (endDate model) a.date) activities
    |> List.map (\a -> Activity.init a |> Many.Add |> activitiesC |> toCmd)


endDate : Model -> Date
endDate model =
    Date.add Week 1 model.date

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdaterMsg u ->
            u model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW

view : Model -> Html Msg
view model =
    div [ class "week" ] (model.activities.viewAll (\ id activity conv -> Activity.view activity |> conv |> Just))
        |> Html.map activitiesC


viewAggregate : Model -> Svg Msg
viewAggregate model =
    let
        typeVolumes = activitiesVolume model
        totalWidth = (List.map Tuple.second typeVolumes |> List.sum) |> logBase 2 |> (*) 10
    in
        svg
            [ width <| toString totalWidth, height <| toString totalWidth ]
            (typeVolumes 
                |> treemapCoordinates totalWidth totalWidth
                |> List.map (\(t, (x_, y_, width_, height_)) -> 
                    rect 
                        [ x_ |> toString |> x
                        , y_ |> toString |> y
                        , width_ |> toString |> width
                        , height_ |> toString |> height
                        , t |> Activity.color |> Svg.Attributes.fill
                        ]
                        []
                ))


treemapCoordinates : Float -> Float -> List (ActivityType, Float) -> List (ActivityType, Coordinate)
treemapCoordinates w h activitiesVolumes =
    let
        coordinates = List.map Tuple.second activitiesVolumes
            |> Treemap.treemapSingledimensional (Treemap.Container (Treemap.Offset 0 0) w h)
        strings = List.map Tuple.first activitiesVolumes
    in
        List.map2 (,) strings coordinates


activitiesVolume : Model -> List (ActivityType, Float)
activitiesVolume model =
   List.map (\t -> (t, (activitiesWidth model t) * (activitiesHeight model t) |> toFloat))  activityTypes


activitiesWidth : Model -> ActivityType -> Int
activitiesWidth model activityType =
    List.filter (\a -> a.type_ == activityType) (Dict.values model.activities.objects)
        |> List.map (\a -> a.durationMinutes)
        |> List.sum


activitiesHeight : Model -> ActivityType -> Int
activitiesHeight model activityType = 
    List.filter (\a -> a.type_ == activityType) (Dict.values model.activities.objects)
        |> List.map (\a -> a.intensity)
        |> List.sum