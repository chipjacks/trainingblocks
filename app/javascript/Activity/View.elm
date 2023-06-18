module Activity.View exposing (activityDescription, lapDescription, listItem)

import Activity.Types exposing (ActivityData, LapData(..))
import Duration
import Distance
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, style)
import Html.Events
import Json.Decode as Decode
import MonoIcons
import Msg exposing (Msg(..))
import Pace exposing (StandardPace)
import Pace.List exposing (PaceList)
import UI.Layout exposing (column, compactColumn, row)
import UI.Util exposing (attributeIf, borderStyle, stopPropagationOnClick, styleIf, viewMaybe, viewIf)


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
        elevationGainStr gainM =
            case gainM of
                Just gain ->
                    " with " ++ (metersToFeet gain |> Basics.round |> String.fromInt) ++ "ft of climbing"

                Nothing ->
                    ""

        metersToFeet meters =
            meters * 3.28084

        isMostlyHills distance gain =
            (gain / distance) > thresholdVertFeetPerMile

        thresholdVertFeetPerMile =
            125 / 5280
    in
    String.join " "
        [ Maybe.map Duration.toStringWithUnits data.duration |> Maybe.withDefault ""
        , if Maybe.map2 isMostlyHills data.distance data.elevationGain == Just True then
            elevationGainStr data.elevationGain
          else
            trainingPaceStr data.pace
        ]


listItem :
    { titleM : Maybe String
    , subtitle : String
    , importM : Maybe String
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
            [ row [ style "overflow-wrap" "break-word", style "color" "var(--black-500)" ]
                [ compactColumn [] [ viewMaybe params.titleM text ]
                , viewMaybe params.importM (\_ -> compactColumn [ style "margin-left" "5px" ] [ MonoIcons.icon (MonoIcons.linkAlt "var(--grey-900)") ])
                ]
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
