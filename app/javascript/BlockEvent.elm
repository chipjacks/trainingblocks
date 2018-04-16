module BlockEvent exposing (State, Event(..), update, view, init)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Mouse
import Block exposing (Data(..))
import Activity
import Date.Extra as Date


type State
    = Viewing Mouse.Event Block.Model
    | Editing Mouse.Event Block.Model
    | None


type Event
    = View Mouse.Event Block.Model
    | Create Mouse.Event
    | Edit Mouse.Event Block.Model
    | Save Mouse.Event
    | Escape


init : State
init =
    None


update : Event -> State -> State
update event state =
    case event of
        View mouse block ->
            case state of
                Editing _ _ ->
                    state

                _ ->
                    Viewing mouse block

        Edit mouse block ->
            Editing mouse block

        Escape ->
            case state of
                Viewing _ _ ->
                    None

                _ ->
                    state

        _ ->
            None


view : State -> Html msg
view state =
    case state of
        Viewing mouseEvent blockModel ->
            div
                [ class "block-tooltip"
                , Html.Attributes.style (positionTooltip mouseEvent.pagePos mouseEvent.offsetPos blockModel.w)
                ]
                [ viewTooltip blockModel.data ]

        Editing mouseEvent blockModel ->
            div
                [ class "block-tooltip"
                , Html.Attributes.style (positionTooltip mouseEvent.pagePos mouseEvent.offsetPos blockModel.w)
                ]
                [ viewForm blockModel.data ]

        None ->
            div [] []



-- INTERNAL


positionTooltip : ( Float, Float ) -> ( Float, Float ) -> Int -> List ( String, String )
positionTooltip pagePos offsetPos blockWidth =
    [ ( "top", toString ((Tuple.second pagePos) - (Tuple.second offsetPos)) ++ "px" )
    , ( "left", toString ((Tuple.first pagePos) - (Tuple.first offsetPos) + (toFloat blockWidth) + 5) ++ "px" )
    ]


viewTooltip : Block.Data -> Html msg
viewTooltip data =
    case data of
        Activity activity ->
            div [ class "ui card" ]
                [ div [ class "content" ]
                    [ div [ class "header" ] [ Html.text activity.name ]
                    , div [ class "ui list" ]
                        [ div [ class "item" ] [ Html.text <| (toString activity.type_) ]
                        , div [ class "item" ] [ Html.text <| (Date.toFormattedString "h:mm a" activity.startDate) ]
                        , div [ class "item" ] [ Html.text <| ((toString (activity.duration // 60)) ++ " minutes") ]
                        , div [ class "item" ] [ Html.text <| Activity.miles activity ]
                        , div [ class "item" ] [ Html.text <| Activity.pace activity ]
                        ]
                    ]
                ]

        Blocks blocks ->
            div [] []


viewForm : Block.Data -> Html msg
viewForm data =
    case data of
        Activity activity ->
            div [ class "ui card" ]
                [ div [ class "content" ]
                    [ div [ class "header" ] [ Html.text activity.name ]
                    , Html.text "Editing"
                    ]
                ]

        Blocks blocks ->
            div [] []
