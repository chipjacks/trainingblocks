module Activity exposing (..)

import StravaAPI exposing (StravaAPIActivity)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Date exposing (Date)
import Date.Extra as Date
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (width, height, x, y, stroke, strokeWidth)
import Treemap exposing (Coordinate, Container)


-- MODEL


type alias Model =
    { type_ : ActivityType
    , date : Date
    , intensity : Int -- 1 to 5, either inferred from pace or user defined
    , durationMinutes : Int -- in minutes... TODO: change it to seconds.

    --    , externalId: Maybe String -- if user attaches an activity
    --    , completed: Bool -- could be completed without necessarily having an external activity
    --    , notes: List Note
    }


type ActivityType
    = Run
    | Ride
    | Weights
    | Swim
    | Other


activityTypes : List ActivityType
activityTypes =
    [ Run, Ride, Weights, Swim, Other ]


type Msg
    = NoOp


init : Model -> ( Model, Cmd Msg )
init activity =
    ( activity, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


fromStravaAPIActivity : StravaAPIActivity -> Model
fromStravaAPIActivity activity =
    Model (toType activity.type_) activity.date (toIntensity activity) (activity.duration // 60)


toIntensity : StravaAPIActivity -> Int
toIntensity activity =
    let
        -- TODO: what about activites that aren't runs?
        minPerMile =
            (((toFloat activity.duration) / 60) / (toMiles activity.distance))

        estimatedIntensity =
            (9 - Basics.floor minPerMile)
    in
        if estimatedIntensity < 1 then
            1
        else if estimatedIntensity > 5 then
            5
        else
            estimatedIntensity


toMiles : Float -> Float
toMiles meters =
    meters / 1609.34


toType : String -> ActivityType
toType str =
    case str of
        "Run" ->
            Run

        "Ride" ->
            Ride

        "WeightTraining" ->
            Weights

        "Swim" ->
            Swim

        _ ->
            Other


volume : Model -> Float
volume model =
    model.durationMinutes * model.intensity |> toFloat


color : ActivityType -> String
color type_ =
    case type_ of
        Run ->
            "skyblue"

        Weights ->
            "red"

        Ride ->
            "green"

        Swim ->
            "orange"

        Other ->
            "grey"


groupByType : List Model -> List (List Model)
groupByType activities =
    activityTypes
        |> List.map (\t -> List.filter (\a -> a.type_ == t) activities)
        |> List.filter (\l -> List.length l > 0)



-- VIEW


viewTreemap : List Model -> Svg msg
viewTreemap activities =
    let
        totalWidth =
            List.map .durationMinutes activities
                |> List.sum
                |> toFloat
                |> (\n -> n / 10)

        totalHeight =
            List.map .intensity activities
                |> List.sum
                |> toFloat
                |> (*) 5
    in
        svg
            [ width <| toString totalWidth, height <| toString totalHeight ]
            (treemapCoordinates totalWidth totalHeight activities
                |> List.map
                    (\( t, ( x_, y_, width_, height_ ) ) ->
                        rect
                            [ x_ |> toString |> x
                            , y_ |> toString |> y
                            , width_ |> toString |> width
                            , height_ |> toString |> height
                            , t |> color |> Svg.Attributes.fill
                            , stroke "white"
                            , strokeWidth "2"
                            ]
                            []
                    )
            )


treemapCoordinates : Float -> Float -> List Model -> List ( ActivityType, Coordinate )
treemapCoordinates w h activities =
    let
        coordinates =
            List.map volume activities
                |> Treemap.treemapSingledimensional (Treemap.Container (Treemap.Offset 0 0) w h)

        types =
            List.map .type_ activities
    in
        List.map2 (,) types coordinates
