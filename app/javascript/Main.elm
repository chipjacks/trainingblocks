module Main exposing (..)

import Html exposing (Html, div, span, button, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Date exposing (Date, Month(..))
import Date.Extra as Date exposing (Interval(..))
import Updater exposing (Updater, Converter, converter, noReaction, toCmd)
import Updater.Many as Many
import Month


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias MonthsModel =
    Many.Model Month.Model Month.Msg


type alias MonthsMsg =
    Many.Msg Month.Model Month.Msg


type alias Model =
    { months : MonthsModel
    , loadOlderDate : Date
    }


type Msg
    = UpdaterMsg (Updater Model Msg)
    | LoadOlderMonthMsg


monthsC : Converter Msg MonthsMsg
monthsC =
    converter
        UpdaterMsg
        { get = Just << .months
        , set = (\cm model -> { model | months = cm })
        , update = Many.update
        , react = noReaction
        }



-- INIT


init : ( Model, Cmd Msg )
init =
    { months = Many.initModel Month.update Month.subscriptions
    , loadOlderDate = Date.fromCalendarDate 2017 Mar 1
    } ! (loadMonths <| Date.fromCalendarDate 2018 Apr 1)


loadMonths : Date -> List (Cmd Msg)
loadMonths date =
        let
            maxMonth = Date.floor Date.Month date
            minMonth = Date.add Date.Month -6 maxMonth
        in
            Date.range Date.Month 1 minMonth maxMonth
                |> List.map (\d -> toCmd <| monthsC <| Many.Add <| Month.init d)


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdaterMsg u ->
            u model

        LoadOlderMonthMsg ->
            let
                olderDate =
                    Date.add Month -1 model.loadOlderDate
            in
                ( { model | loadOlderDate = olderDate }, toCmd <| monthsC <| Many.Add <| Month.init model.loadOlderDate )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "main" ] 
        [ (model.months.viewAll (\id month conv -> Month.view month |> conv |> Just)) |> div [ class "months" ] |> Html.map monthsC
        , div [] [ button [ onClick LoadOlderMonthMsg ] [ text "Add Month" ] ]
        ]