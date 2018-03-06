module Container exposing (..)

import RemoteData exposing (WebData, RemoteData(..))
import Date exposing (Date)
import Date.Extra as Date exposing (Interval(..), isBetween)
import StravaAPI exposing (StravaAPIActivity)
import Activity exposing (fromStravaAPIActivity)
import Html exposing (Html, div, span)
import Html.Attributes exposing (style)


type alias Model =
    { -- id: BlockId
      --   , userId: UserId,
      blocks : WebData (List Block)

    -- , scale: Date.Interval
    , date : Date

    -- Maybe add some aggregated metrics here so recursive load can happen lazily
    }


type Block
    = ActivityBlock Activity.Model
    | ContainerBlock Model


type alias BlockId =
    String


init : Date -> ( Model, Cmd Msg )
init date =
    let
        model =
            { blocks = NotAsked, date = date }
    in
        ( model, loadBlocks model )



-- UPDATE


type Msg
    = GotStravaActivities (WebData (List StravaAPIActivity))
    | FetchBlocks


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchBlocks ->
            ( model, (loadBlocks model) )

        GotStravaActivities webdata ->
            case webdata of
                Success activities ->
                    let
                        blocks =
                            List.map fromStravaAPIActivity activities
                              -- TODO: the API should do this filtering
                              |> List.filter (\a -> isBetween model.date (endDate model) a.date)
                              |> List.map ActivityBlock
                    in
                        ( { model | blocks = Success blocks }, Cmd.none )

                Failure msg ->
                    ( model, Cmd.none )

                Loading ->
                    ( { model | blocks = Loading }, Cmd.none )

                NotAsked ->
                    ( { model | blocks = NotAsked }, Cmd.none )


loadBlocks : Model -> Cmd Msg
loadBlocks model =
    let
        startDate =
            model.date
    in
        StravaAPI.listActivities startDate (endDate model)
            |> RemoteData.sendRequest
            |> Cmd.map GotStravaActivities


endDate : Model -> Date
endDate model =
    Date.add Month 1 model.date

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model.blocks of
        RemoteData.Success loadedBlocks ->
            div [ style [ ( "height", "120px" ) ] ]
                ((span [] [ Html.text ((Date.toFormattedString "MMM d" model.date) ++ " - " ++ (Date.toFormattedString "MMM d" <| endDate model)) ])
                    :: (List.map viewBlock loadedBlocks)
                )

        _ ->
            Html.text ""


viewBlock : Block -> Html Msg
viewBlock block =
    case block of
        ActivityBlock activity ->
            Activity.view activity

        ContainerBlock block ->
            Html.text ""
