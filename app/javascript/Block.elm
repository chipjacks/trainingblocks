module Block exposing (Model, initModel, Data(..), View(..), sum, scale, split, stack, decompose, normalize, normalizer, plot, shift, crop, list)

import Activity exposing (Activity, ActivityType(..))
import Date.Extra as Date


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
    = Activity Activity
    | Blocks Blocks


type alias Blocks =
    List Model


initModel : Data -> Model
initModel data =
    case data of
        Blocks blocks ->
            let
                maxX =
                    blocks |> List.map (\b -> b.x + b.w) |> List.maximum |> Maybe.withDefault 0

                maxY =
                    blocks |> List.map (\b -> b.y + b.h) |> List.maximum |> Maybe.withDefault 0
            in
                Model 0 0 maxX maxY "" data Children

        Activity activity ->
            Model 0 0 (activity.duration // 60) (toIntensity activity) (color activity.type_) data Normal



-- TRANSFORMERS


shift : Int -> Int -> Model -> Model
shift xOffset yOffset model =
    { model | x = model.x + xOffset, y = model.y + yOffset }


plot : Model -> Model
plot model =
    case model.data of
        Activity activity ->
            shift (activity.startDate |> Date.fractionalDay |> (*) 24 |> (*) 38 |> round) 0 model

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


crop : Int -> Int -> Model -> Model
crop maxW maxH model =
    let
        newW =
            List.minimum [ maxW, model.w ] |> Maybe.withDefault model.w

        newH =
            List.minimum [ maxH, model.h ] |> Maybe.withDefault model.h
    in
        { model | w = newW, h = newH }



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

        w_ =
            blocks |> List.map .w |> List.sum

        h_ =
            (activities model |> List.map toIntensity |> List.sum) // (activities model |> List.length)
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


list : Blocks -> Model
list blocks =
    blocks
        |> List.foldl listOnParent (initModel (Blocks []))



-- INTERNAL


activities : Model -> List Activity
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


listOnParent : Model -> Model -> Model
listOnParent block parent =
    let
        newBlock =
            case (lastBlock parent) of
                Just lastBlock ->
                    { block | y = (lastBlock.y + lastBlock.h + 5) }

                Nothing ->
                    block
    in
        appendBlock newBlock parent


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


color : ActivityType -> String
color type_ =
    case type_ of
        Run ->
            "skyblue"

        Weights ->
            "grey"

        Ride ->
            "grey"

        Swim ->
            "grey"

        Other ->
            "grey"


toIntensity : Activity -> Int
toIntensity activity =
    let
        minPerMile =
            (((toFloat activity.duration) / 60) / (activity.distance / 1609.34))

        estimatedIntensity =
            (9 - Basics.floor minPerMile)
    in
        if activity.type_ /= Run then
            1
        else if estimatedIntensity < 1 then
            1
        else if estimatedIntensity > 5 then
            5
        else
            estimatedIntensity
