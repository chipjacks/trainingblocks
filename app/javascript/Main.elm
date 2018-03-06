module Main exposing (..)

import Html exposing (Html, div, span, button, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Date exposing (Date, Month(..))
import Date.Extra as Date exposing (Interval(..))
import Updater exposing (Updater, Converter, converter, noReaction, toCmd)
import Updater.Many as Many
import Container exposing (BlockId)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias ContainersModel =
    Many.Model Container.Model Container.Msg


type alias ContainersMsg =
    Many.Msg Container.Model Container.Msg


type alias Model =
    { containers : ContainersModel
    , loadOlderDate : Date
    }


type Msg
    = UpdaterMsg (Updater Model Msg)
    | LoadOlderContainerMsg


containersC : Converter Msg ContainersMsg
containersC =
    converter
        UpdaterMsg
        { get = Just << .containers
        , set = (\cm model -> { model | containers = cm })
        , update = Many.update
        , react = noReaction
        }



-- INIT


init : ( Model, Cmd Msg )
init =
    { containers = Many.initModel Container.update Container.subscriptions
    , loadOlderDate = Date.fromCalendarDate 2018 Mar 1
    }
        ! []



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdaterMsg u ->
            u model

        LoadOlderContainerMsg ->
            let
                olderDate =
                    Date.add Week -1 model.loadOlderDate
            in
                ( { model | loadOlderDate = olderDate }, toCmd <| containersC <| Many.Add <| Container.init model.loadOlderDate )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ (div [] [ button [ onClick LoadOlderContainerMsg ] [ text "Add Container" ] ])
        , (Html.map containersC <| viewContainers model.containers)
        ]


viewContainers : ContainersModel -> Html ContainersMsg
viewContainers containers =
    div [ class "timers" ]
        [ div [ style [ ( "height", "420px" ) ] ] <|
            containers.viewAll
                (\id container conv ->
                    Just <|
                        div
                            [ style
                                [ ( "width", "215px" )
                                , ( "float", "left" )
                                , ( "height", "320px" )
                                ]
                            ]
                            [ conv <| Container.view container
                            , button [ onClick <| Many.Delete id ] [ text "Delete" ]
                            ]
                )
        ]



-- viewLoadMoreButton : Model -> Html Msg
-- viewLoadMoreButton model =
-- button [ onClick ((ActivitiesMsg << FetchActivities) (prevMonthStart model)) ] [ Html.text "Load More" ]
