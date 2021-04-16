module Activity.View exposing (listItem)

import Activity.Types exposing (ActivityData)
import Duration
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, style)
import Html.Events
import Json.Decode as Decode
import Msg exposing (ActivityConfigs, Msg(..))
import Pace
import Skeleton exposing (attributeIf, borderStyle, column, compactColumn, onPointerDown, row, styleIf, viewMaybe)


listItem :
    ActivityConfigs
    ->
        { descriptionM : Maybe String
        , data : ActivityData
        , isActive : Bool
        , handlePointerDown : Decode.Decoder Msg
        , handleDoubleClick : Msg
        , handleMultiSelectM : Maybe (Decode.Decoder Msg)
        , viewToolbarM : Maybe (Html Msg)
        , viewShape : Html Msg
        }
    -> Html Msg
listItem configs params =
    let
        isActive =
            params.isActive

        data =
            params.data

        trainingPaceStr paceM =
            case ( paceM, configs.levelM ) of
                ( Just pace, Just level ) ->
                    Pace.secondsToTrainingPace level pace
                        |> Pace.trainingPace.toString
                        |> String.toLower

                ( Just pace, Nothing ) ->
                    " at " ++ Pace.paceToString pace ++ " pace"

                _ ->
                    ""
    in
    row
        [ style "padding" "0.5rem 0.5rem"
        , styleIf isActive "background-color" "var(--grey-100)"
        , style "position" "relative"
        , Html.Events.onDoubleClick params.handleDoubleClick
        , attributeIf isActive (onPointerDown (Decode.succeed NoOp))
        ]
        [ viewMaybe params.viewToolbarM
            (\toolbar ->
                div
                    [ style "position" "absolute"
                    , style "top" "-20px"
                    , style "right" "0"
                    ]
                    [ toolbar
                    ]
            )
        , compactColumn
            [ style "flex-basis" "5rem"
            , style "justify-content" "center"
            , attributeIf (not isActive) (onPointerDown params.handlePointerDown)
            ]
            [ params.viewShape ]
        , a
            [ class "column expand"
            , style "justify-content" "center"
            , attributeIf (not isActive) (onPointerDown params.handlePointerDown)
            ]
            [ row [ style "word-break" "break-all" ] [ viewMaybe params.descriptionM text ]
            , row [ style "font-size" "0.8rem" ]
                [ column []
                    [ text <|
                        String.join " "
                            [ Maybe.map Duration.toStringWithUnits data.duration |> Maybe.withDefault ""
                            , trainingPaceStr data.pace
                            ]
                    ]
                ]
            ]
        , viewMaybe params.handleMultiSelectM
            (\handleMultiSelect ->
                compactColumn
                    [ attributeIf (not isActive)
                        (onPointerDown handleMultiSelect)
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
            )
        ]
