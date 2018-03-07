module Week exposing (..)

import Updater exposing (Updater, Converter, converter, noReaction, toCmd)
import Updater.Many as Many
import Activity
import Date exposing (Date)
import Date.Extra as Date exposing (toFormattedString, Interval(..), isBetween)
import Html exposing (Html, div, span, text, button)
import Html.Attributes exposing (style)

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
    div [ style [ ( "height", "25%" ) ] ] [(Html.map activitiesC <| viewActivities model.activities)]

viewCompact : Model -> Html Msg
viewCompact model =
    div [ style [ ( "height", "30px" ) ] ] [(Html.map activitiesC <| viewActivities model.activities)]

viewActivities : ActivitiesModel -> Html ActivitiesMsg
viewActivities activities = 
    div [ ]
        [ div [ style [] ] <|
              activities.viewAll
              (\ id activity conv -> Just <| conv <| Activity.view activity)
        ]
