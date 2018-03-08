module Main exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Months


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Page
    = Months Months.Model


type alias Model =
    { page : Page
    }


type Msg
    = MonthsMsg Months.Msg


-- INIT


init : ( Model, Cmd Msg )
init =
    let
        (subModel, subCmd) = Months.init
    in
        { page = Months subModel
        } ! [subCmd |> Cmd.map MonthsMsg]


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.page of
        Months page ->
            case msg of
                MonthsMsg msg ->
                    let
                        (newMonths, newCmd) = Months.update msg page
                    in
                        ({ model | page = Months newMonths}, newCmd |> Cmd.map MonthsMsg)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model.page of
        Months months ->
            div [ class "main" ] [ Months.view months |> Html.map MonthsMsg ]