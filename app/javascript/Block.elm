module Block exposing (Model, initModel, Data(..), View(..), sum, scale, split, stack, decompose, normalize, normalizer, plot, Event(..), shift)

import Activity
import Date
import Date.Extra


{-
   - containers (combine several blocks/containers into one)
       - session (laps w/ different intensities)
       x sum (average intensity)
       - treemap
       x stack
   - scalers (change block width, height)
       - normalize
       - timeline
       x split
   - shifters (change block x, y)
       - plot
   - events (block events, msgs)
       - mouseover tooltip
       - mouseover unstack
       - click
       - drag/drop
       - stretch
   - labels (extras positioned relative to block)
       - distance
       - intensity
       - time
       - notes
       - tooltip
-}


type alias Model =
    { x : Int
    , y : Int
    , w : Int
    , h : Int
    , color : String
    , data : Data
    , view : View
    }


type View
    = Children
    | Split
    | Normal


type Data
    = Activity Activity.Model
    | Blocks Blocks


type Event
    = MouseOver
    | Drag Int Int


type alias Blocks =
    List Model


initModel : Data -> Model
initModel data =
    case data of
        Blocks blocks ->
            Model 0 0 0 0 "" data Children

        Activity activity ->
            Model 0 0 activity.durationMinutes activity.intensity (Activity.color activity.type_) data Normal



-- TRANSFORMERS


shift : Int -> Int -> Model -> Model
shift xOffset yOffset model =
    { model | x = model.x + xOffset, y = model.y + yOffset }


plot : Model -> Model
plot model =
    case model.data of
        Activity activity ->
            shift (activity.date |> Date.hour |> (*) 100) (activity.date |> Date.dayOfWeek |> Date.Extra.weekdayToNumber |> (+) -1 |> (*) 100) model

        Blocks blocks ->
            model


scale : Float -> Float -> Model -> Model
scale xFactor yFactor model =
    case model.data of
        Blocks blocks ->
            { model
                | w = ((toFloat model.w) * xFactor) |> round
                , h = ((toFloat model.h) * yFactor) |> round
                , x = ((toFloat model.x) * xFactor) |> round
                , y = ((toFloat model.y) * yFactor) |> round
                , data = Blocks (List.map (scale xFactor yFactor) blocks)
            }

        _ ->
            { model
                | w = ((toFloat model.w) * xFactor) |> round
                , h = ((toFloat model.h) * yFactor) |> round
                , x = ((toFloat model.x) * xFactor) |> round
                , y = ((toFloat model.y) * yFactor) |> round
            }


normalize : Blocks -> Blocks
normalize blocks =
    List.map (normalizer blocks) blocks


normalizer : Blocks -> (Model -> Model)
normalizer blocks =
    case (List.map .w blocks |> List.maximum) of
        Just maxDuration ->
            scale (1 / (toFloat maxDuration) * 100) 1

        Nothing ->
            scale 1 1



-- DECOMPOSERS


decompose : Model -> Blocks
decompose model =
    case model.data of
        Blocks blocks ->
            blocks

        Activity activity ->
            []


split : Int -> Model -> Blocks
split maxWidth model =
    if model.w > maxWidth then
        { model | w = maxWidth, view = Split } :: (split maxWidth { model | w = model.w - maxWidth })
    else
        [ model ]



-- COMPOSERS


sum : Blocks -> Model
sum blocks =
    let
        model =
            initModel (Blocks blocks)
        w_ = blocks |> List.map .w |> List.sum
        h_ = (activities model |> List.map .intensity |> List.sum) // (activities model |> List.length)
    in
        { model
            | w = w_
            , h = h_
            , color = (blocks |> List.map .color |> List.head |> Maybe.withDefault "")
            , view = Normal
        }


stack : Blocks -> Model
stack blocks =
    blocks
        |> List.foldl stackOnParent (initModel (Blocks []))



-- INTERNAL


activities : Model -> List Activity.Model
activities model =
    case model.data of
        Blocks blocks ->
            List.concatMap activities blocks

        Activity activity ->
            [ activity ]


stackOnParent : Model -> Model -> Model
stackOnParent block parent =
    appendBlock
        (case (lastBlock parent) of
            Just lastBlock ->
                if lastBlock.view == Split then
                    { block | x = lastBlock.x + 2, y = lastBlock.y + 3 }
                else
                    { block | x = lastBlock.x + 5, y = lastBlock.y + 5 }

            Nothing ->
                block
        )
        parent


appendBlock : Model -> Model -> Model
appendBlock model parent =
    (decompose parent)
        ++ [ model ]
        |> initModel
        << Blocks


lastBlock : Model -> Maybe Model
lastBlock model =
    decompose model |> List.reverse |> List.head


volume : Model -> Float
volume model =
    (model.w * model.h) |> toFloat
