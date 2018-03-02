module Main exposing (..)

import Html exposing (Html, div, span, button)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StravaAPI exposing (..)
import Msgs exposing (..)
import Models exposing (..)
import Date exposing (Date, Month(..))
import Date.Extra as Date
import Dict exposing (Dict)
import RemoteData exposing (RemoteData, WebData)
import Task exposing (Task)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


init : ( Model, Cmd Msg )
init =
    ( Model Dict.empty [] Nothing, (Task.attempt getLatestActivities Date.now) )


prevMonthStart : Model -> Date
prevMonthStart model =
    Dict.keys model.activities
        |> List.sort
        |> List.head
        |> Maybe.map Date.fromRataDie
        -- TODO: better error handling
        |> Maybe.withDefault (Date.fromCalendarDate 2018 Jan 1)
        |> Date.add Date.Month -1


getLatestActivities : Result String Date -> Msg
getLatestActivities result =
    case result of
        Ok date ->
            (AActivitiesMsg << FetchActivities) (Date.floor Date.Month date)

        Err _ ->
            -- TODO: better error handling
            (AActivitiesMsg << FetchActivities) (Date.fromCalendarDate 2018 Jan 1)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AActivitiesMsg activitiesMsg ->
            updateActivitiesMsg activitiesMsg model


updateActivitiesMsg : ActivitiesMsg -> Model -> ( Model, Cmd Msg )
updateActivitiesMsg msg model =
    case msg of
        FetchActivities date ->
            ( model, (loadMonthsActivities date) )

        GotActivities rataDie activities ->
            let
                newActivities =
                    Dict.insert rataDie activities model.activities
            in
                ( { model | activities = newActivities }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        weeks =
            Dict.toList model.activities |> List.sortBy Tuple.first |> List.reverse
    in
        div [] ((List.map viewRow weeks) ++ [ (viewLoadMoreButton model) ])


viewLoadMoreButton : Model -> Html Msg
viewLoadMoreButton model =
    button [ onClick ((AActivitiesMsg << FetchActivities) (prevMonthStart model)) ] [ Html.text "Load More" ]


viewRow : ( RataDie, WebData (List StravaAPIActivity) ) -> Html Msg
viewRow weeksActivities =
    let
        startDate =
            Date.fromRataDie (Tuple.first weeksActivities)

        endDate =
            Date.add Date.Week 1 startDate
    in
        case weeksActivities of
            ( rataDie, RemoteData.Success loadedActivities ) ->
                div [ style [ ( "height", "120px" ) ] ]
                    ((span [] [ Html.text ((Date.toFormattedString "MMM d" startDate) ++ " - " ++ (Date.toFormattedString "MMM d" endDate)) ])
                        :: ((Date.range Date.Day 1 startDate endDate)
                                |> List.map (\d -> ( d, List.filter (\r -> (Date.toRataDie r.date) == (Date.toRataDie d)) loadedActivities ))
                                |> List.map (\( d, rs ) -> Day d (List.map toActivity rs))
                                |> List.map (\b -> viewDay b)
                           )
                    )

            _ ->
                Html.text ""


viewDay : Day -> Html Msg
viewDay day =
    div [ style [ ( "display", "inline-block" ) ] ]
        [ viewAnnotations day
        , div [] (List.map viewActivity day.blocks)
        ]


viewActivity : Activity -> Html Msg
viewActivity block =
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


viewAnnotations : Day -> Html Msg
viewAnnotations day =
    div [ style [ ( "height", "20px" ) ] ] [ Html.text "" ]
