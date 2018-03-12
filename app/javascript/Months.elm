module Months exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html as Html
import Dict exposing (Dict)
import Date exposing (Date, Month(..))
import Date.Extra as Date exposing (Interval(..))
import Updater exposing (converter, Updater, Converter, Interface, toCmd, noReaction)
import Month


type alias Model =
    { months : Dict Int Month.Model }


type Msg
    = AddMonth ( Month.Model, Cmd Month.Msg )
    | LoadEarlier
    | UpdaterMsg (Updater Model Msg)


monthC : Int -> Converter Msg Month.Msg
monthC n =
    converter
        UpdaterMsg
        { get = (\model -> Dict.get n model.months)
        , set = (\month model -> { model | months = Dict.insert n month model.months })
        , update = Month.update
        , react = noReaction
        }



-- INIT


init : ( Model, Cmd Msg )
init =
    { months = Dict.empty }
        ! (loadMonths <| Date.fromCalendarDate 2018 Apr 1)


loadMonths : Date -> List (Cmd Msg)
loadMonths date =
    let
        maxMonth =
            Date.floor Date.Month date

        minMonth =
            Date.add Date.Month -6 maxMonth
    in
        Date.range Date.Month 1 minMonth maxMonth
            |> List.map (\d -> toCmd <| AddMonth <| Month.init d)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadEarlier ->
            let
                newDate =
                    Dict.keys model.months |> List.minimum |> Maybe.withDefault 1 |> Date.fromRataDie |> Date.add Date.Month -1

                newId =
                    newDate |> Date.toRataDie

                ( newMonth, newCmd ) =
                    Month.init newDate
            in
                { model | months = Dict.insert newId newMonth model.months }
                    ! [ Cmd.map (monthC newId) <| newCmd ]

        AddMonth ( monthModel, monthCmd ) ->
            let
                newId =
                    monthModel.date |> Date.toRataDie
            in
                { model | months = Dict.insert newId monthModel model.months }
                    ! [ Cmd.map (monthC newId) <| monthCmd ]

        UpdaterMsg u ->
            u model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        List.map
            (\( id, monthModel ) ->
                Sub.map (monthC id) <| Month.subscriptions monthModel
            )
            (Dict.toList model.months)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ class "months" ] <| List.map (\( id, monthModel ) -> Html.map (monthC id) <| Month.view monthModel) (Dict.toList model.months)
        , div [] [ button [ onClick LoadEarlier ] [ text "Add Month" ] ]
        ]
