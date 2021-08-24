module Activity.View exposing (activityDescription, lapDescription, listItem)

import Activity.Types exposing (ActivityData, LapData(..))
import Duration
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, style)
import Html.Events
import Json.Decode as Decode
import Msg exposing (Msg(..))
import Pace exposing (StandardPace)
import Pace.List exposing (PaceList)
import UI.Layout exposing (column, compactColumn, row)
import UI.Util exposing (attributeIf, borderStyle, stopPropagationOnClick, styleIf, viewMaybe)


lapDescription : Maybe (PaceList StandardPace) -> LapData -> String
lapDescription pacesM lap =
    case lap of
        Individual data ->
            activityDescription pacesM data

        Repeats count list ->
            String.join " "
                [ String.fromInt count
                , "x"
                , String.join ", " (List.map (activityDescription pacesM) list)
                ]


activityDescription : Maybe (PaceList StandardPace) -> ActivityData -> String
activityDescription _ data =
    let
        trainingPaceStr paceM =
            case paceM of
                Just pace ->
                    " at " ++ Pace.paceToString pace ++ " pace"

                Nothing ->
                    ""
    in
    String.join " "
        [ Maybe.map Duration.toStringWithUnits data.duration |> Maybe.withDefault ""
        , trainingPaceStr data.pace
        ]


listItem :
    { titleM : Maybe String
    , subtitle : String
    , isActive : Bool
    , handlePointerDown : Decode.Decoder Msg
    , handleDoubleClick : Msg
    , handleMultiSelectM : Maybe (Decode.Decoder Msg)
    , viewToolbarM : Maybe (Html Msg)
    , viewShape : Html Msg
    }
    -> Html Msg
listItem params =
    let
        isActive =
            params.isActive
    in
    row
        [ style "padding" "0.5rem 0.5rem"
        , style "min-height" "3rem"
        , styleIf isActive "background-color" "var(--grey-100)"
        , style "border-radius" "5px"
        , style "position" "relative"
        , Html.Events.stopPropagationOn "dblclick" (Decode.succeed ( params.handleDoubleClick, True ))
        , stopPropagationOnClick (Decode.succeed NoOp) -- prevents close event from firing
        , style "max-width" "100%"
        ]
        [ viewMaybe params.viewToolbarM
            (\toolbar ->
                div
                    [ style "position" "absolute"
                    , style "bottom" "-15px"
                    , style "right" "0"
                    , style "z-index" "3"
                    ]
                    [ toolbar
                    ]
            )
        , compactColumn
            [ style "flex-basis" "5rem"
            , style "justify-content" "center"
            , attributeIf (not isActive) (stopPropagationOnClick params.handlePointerDown)
            ]
            [ params.viewShape ]
        , column
            [ style "justify-content" "center"
            , style "overflow" "hidden"
            , attributeIf (not isActive) (stopPropagationOnClick params.handlePointerDown)
            ]
            [ div [ style "overflow-wrap" "break-word", style "color" "var(--black-500)" ] [ viewMaybe params.titleM text ]
            , row [ style "font-size" "0.8rem", style "color" "var(--black-300)" ] [ column [] [ text params.subtitle ] ]
            ]
        , case params.handleMultiSelectM of
            Just handleMultiSelect ->
                compactColumn
                    [ attributeIf (not isActive)
                        (stopPropagationOnClick handleMultiSelect)
                    , style "justify-content" "center"
                    ]
                    [ row
                        [ style "width" "0.4rem"
                        , style "height" "0.4rem"
                        , style "border-radius" "50%"
                        , style "border" "2px solid transparent"
                        , borderStyle "border"
                        , attributeIf isActive (style "background-color" "var(--grey-900)")
                        ]
                        []
                    ]

            Nothing ->
                compactColumn [ style "width" "0.4rem" ] []
        ]
