module Block exposing (initModel, Data(..), sum, scale, split, stack, decompose)

import Activity
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
    { x: Int
    , y: Int
    , w: Int
    , h: Int
    , split: Bool
    , data: Data
    }


type Data
    = Activity (Activity.Model)
    | Blocks Blocks

type alias Blocks = List Model


initModel : Data -> Model 
initModel data =
    case data of
        Blocks blocks ->
            Model 0 0 0 0 False data

        Activity activity ->
            Model 0 0 activity.durationMinutes activity.intensity False data


-- TRANSFORMERS

shift : Int -> Int -> Model -> Model
shift xOffset yOffset model =
    { model | x = model.x + xOffset, y = model.y + yOffset }


scale : Int -> Int -> Model -> Model
scale xFactor yFactor model =
    case model.data of
        Blocks blocks -> 
            { model 
                | w = List.sum (blocks |> List.map (scale xFactor yFactor) |> List.map .w)
                , h = List.sum (blocks |> List.map (scale xFactor yFactor) |> List.map .h)
            }

        Activity activity ->
            { model | w = model.w * xFactor, h = model.h * yFactor }


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
        { model | w = maxWidth, split = True} :: (split maxWidth { model | w = model.w - maxWidth })
    else
        [model]


-- COMPOSERS

sum : Blocks -> Model
sum blocks =
    let
        model = initModel (Blocks blocks)
    in
        { model
            | w = blocks |> List.map .w |> List.sum
            , h = (activities model |> List.map .intensity |> List.sum) // (activities model |> List.length)
        }


stack : Blocks -> Model
stack blocks =
    blocks
        |> List.indexedMap (\i b -> shift (i * 5) (i * 5) b)
        |> initModel << Blocks


-- INTERNAL

activities : Model -> List Activity.Model
activities model =
    case model.data of
        Blocks blocks -> 
            List.concatMap activities blocks

        Activity activity ->
            [activity]