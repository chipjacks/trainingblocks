module Page.Settings exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.Events
import Json.Decode as Decode
import MonoIcons
import Pace
import Ports
import Selection exposing (Selection)
import UI.Button as Button
import UI.Input
import UI.Layout exposing (column, compactColumn, expandingRow, row)
import UI.Navbar as Navbar
import UI.Skeleton as Skeleton
import UI.Util exposing (attributeIf, attributeMaybe, onPointerMove, styleIf, viewMaybe)
import Validate exposing (Field)


main =
    Browser.document
        { init = \x -> init x
        , view = \model -> { title = "Settings | Rhino Log", body = [ view model ] }
        , update = \model msg -> update model msg
        , subscriptions = \_ -> Ports.setDropTarget SetDropTarget
        }


init : () -> ( Model, Cmd msg )
init _ =
    ( Model (Selection.init placeholderPaces) Nothing
    , Cmd.none
    )


placeholderPaces =
    [ ( "Very Easy", "8:00" )
    , ( "Easy", "7:00" )
    , ( "Moderate", "6:30" )
    , ( "Hard", "5:00" )
    ]
        |> List.map
            (\( name, pace ) ->
                ( Validate.init Ok name
                , Validate.init Validate.parsePace pace
                )
            )


type alias Model =
    { trainingPaces : Selection ( Validate.Field String String, Validate.Field String Int )
    , dragging : Maybe ( Float, Float )
    }


type Msg
    = EditedPace Int String
    | EditedName Int String
    | ClickedAddPace
    | ClickedRemovePace Int
    | ClickedDragPace Int Int
    | PointerMoved Float Float
    | SetDropTarget Int
    | PointerUp
    | NoOp


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        EditedPace index str ->
            let
                newTrainingPaces =
                    Selection.select index model.trainingPaces
                        |> Selection.update (Tuple.mapSecond (Validate.update str))
            in
            ( { model | trainingPaces = newTrainingPaces }, Cmd.none )

        EditedName index str ->
            let
                newTrainingPaces =
                    Selection.select index model.trainingPaces
                        |> Selection.update (Tuple.mapFirst (Validate.update str))
            in
            ( { model | trainingPaces = newTrainingPaces }, Cmd.none )

        ClickedAddPace ->
            ( { model | trainingPaces = Selection.add newTrainingPace model.trainingPaces }
            , Cmd.none
            )

        ClickedRemovePace index ->
            let
                newTrainingPaces =
                    Selection.select index model.trainingPaces
                        |> Selection.delete
            in
            ( { model | trainingPaces = newTrainingPaces }
            , Cmd.none
            )

        ClickedDragPace index pointerId ->
            ( { model
                | trainingPaces = Selection.select index model.trainingPaces
                , dragging = Just ( -100, -100 )
              }
            , Ports.setPointerCapture { targetId = trainingPaceListId, pointerId = pointerId }
            )

        PointerMoved x y ->
            ( { model | dragging = Just ( x, y ) }
            , Cmd.none
            )

        PointerUp ->
            ( { model | dragging = Nothing }
            , Cmd.none
            )

        SetDropTarget id ->
            ( { model | trainingPaces = Selection.move id model.trainingPaces }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


newTrainingPace : ( Validate.Field String String, Validate.Field String Int )
newTrainingPace =
    ( Validate.init Ok ""
    , Validate.init Validate.parsePace ""
    )


view : Model -> Html Msg
view model =
    let
        backButton =
            Html.a [ class "button row", style "align-items" "bottom", Html.Attributes.href "/calendar" ]
                [ MonoIcons.icon (MonoIcons.chevronLeft "#3d3d3d")
                , Html.text "Back"
                ]

        navHeader =
            Html.div [ style "font-size" "1.3rem" ] [ Html.text "Settings" ]
    in
    Skeleton.default
        |> Skeleton.withNavbar
            (Navbar.default
                |> Navbar.withBackButton backButton
                |> Navbar.withItems [ navHeader ]
                |> Navbar.view
            )
        |> Skeleton.withBody (viewBody model)
        |> Skeleton.view


viewBody : Model -> Html Msg
viewBody { trainingPaces, dragging } =
    column []
        [ Html.h3 [] [ Html.text "Training Paces" ]
        , row []
            [ viewTrainingPaces dragging (Selection.selectedIndex trainingPaces) (Selection.toList trainingPaces)
            , column [] []
            ]
        , viewMaybe dragging (\position -> viewDraggedPace position trainingPaces)
        ]


trainingPaceListId =
    "training-pace-list"


viewTrainingPaces : Maybe ( Float, Float ) -> Int -> List ( Field String String, Field String Int ) -> Html Msg
viewTrainingPaces dragging selectedIndex paces =
    Html.node "list-dnd"
        [ Html.Attributes.id trainingPaceListId
        , attributeMaybe dragging (\_ -> onPointerMove PointerMoved)
        , attributeMaybe dragging (\_ -> Html.Events.on "pointerup" (Decode.succeed PointerUp))
        ]
        (List.indexedMap (\index fields -> viewPaceForm index ((dragging /= Nothing) && selectedIndex == index) fields) paces ++ [ viewAddButton ])


viewPaceForm : Int -> Bool -> ( Field String String, Field String Int ) -> Html Msg
viewPaceForm index isDropTarget ( name, pace ) =
    row
        [ style "margin-top" "5px"
        , style "margin-bottom" "5px"
        , class "drop-target"
        , styleIf isDropTarget "visibility" "hidden"
        , Html.Attributes.attribute "data-drop-id" (String.fromInt index)
        ]
        [ Button.action "Drag" MonoIcons.drag NoOp
            |> Button.withAttributes
                [ class "row__button--drag"
                , Html.Events.on "pointerdown" (Decode.map (ClickedDragPace index) (Decode.field "pointerId" Decode.int))
                ]
            |> Button.withAppearance Button.Small Button.Subtle Button.None
            |> Button.view
        , UI.Input.text (EditedName index)
            |> UI.Input.withResultError name.result
            |> UI.Input.view name.value
        , compactColumn [ style "width" "10px" ] []
        , UI.Input.pace (EditedPace index)
            |> (\config ->
                    case pace.result of
                        Err Validate.MissingError ->
                            config

                        Err err ->
                            UI.Input.withError err config

                        _ ->
                            config
               )
            |> UI.Input.withPlaceholder (Result.map Pace.paceToString pace.result |> Result.withDefault "mm:ss")
            |> UI.Input.view pace.value
        , Button.action "Remove Pace" MonoIcons.remove (ClickedRemovePace index)
            |> Button.withAppearance Button.Small Button.Subtle Button.Right
            |> Button.view
        ]


viewDraggedPace : ( Float, Float ) -> Selection ( Validate.Field String String, Validate.Field String Int ) -> Html Msg
viewDraggedPace ( x, y ) trainingPaces =
    let
        paceForm =
            viewMaybe
                (Selection.get trainingPaces)
                (\pace ->
                    viewPaceForm (Selection.selectedIndex trainingPaces) False pace
                )
    in
    Html.div
        [ style "position" "absolute"
        , style "left" (String.fromFloat (x - 20) ++ "px")
        , style "top" (String.fromFloat (y - 20) ++ "px")
        , style "opacity" "0.5"
        , Html.Attributes.id "dragged-element"
        ]
        [ paceForm
        ]


viewAddButton : Html Msg
viewAddButton =
    row [ style "justify-content" "flex-end" ]
        [ Button.action "Add Pace" MonoIcons.add ClickedAddPace
            |> Button.withAppearance Button.Small Button.Subtle Button.Right
            |> Button.view
        ]
