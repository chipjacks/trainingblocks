module Activity exposing (..)

import StravaAPI exposing (StravaAPIActivity)
import Html exposing (Html, div)
import Html.Attributes exposing (style)


-- MODEL

type alias Model =
    { -- id : BlockId
      --    , containerId: BlockId
      --    , type_ : ActivityType,
      intensity : Int -- 1 to 5, either inferred from pace or user defined
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




fromStravaAPIActivity : StravaAPIActivity -> Model
fromStravaAPIActivity activity =
    Model (toIntensity activity) (activity.duration // 60)


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


-- VIEW

view : Model -> Html msg
view block =
    div
        [ style
            [ ( "display", "inline-block" )
            , ( "margin-right", "10px" )
            , ( "background", "skyblue" )
            , ( "width", (toString block.durationMinutes) ++ "px" )
            , ( "height", (toString (block.intensity * 10)) ++ "px" )
            ]
        ]
        []