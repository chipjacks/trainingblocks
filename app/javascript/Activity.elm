module Activity exposing (..)

import StravaAPI exposing (StravaAPIActivity)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Date exposing (Date)


-- MODEL


type alias Model =
    { -- id : BlockId
      --    , containerId: BlockId
      type_ : ActivityType
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
            

-- VIEW

view : Model -> Html msg
view block =
    div
        [ style
            [ ( "display", "inline-block" )
            , ( "margin-right", "10px" )
            , ( "background", color block.type_ )
            , ( "width", (toString block.durationMinutes) ++ "px" )
            , ( "height", (toString (block.intensity * 10)) ++ "px" )
            ]
        ]
        []

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
            