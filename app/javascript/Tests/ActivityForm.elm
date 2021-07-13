module Tests.ActivityForm exposing (all)

import Effect exposing (Effect)
import Msg exposing (Msg)
import Page.Calendar exposing (Model, init, view)
import ProgramTest exposing (..)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (class, id, text, attribute)
import Html.Attributes
import Tests.Effects
import Json.Encode as Encode


start : ProgramTest Model Msg Effect
start =
    ProgramTest.createDocument
        { init = \_ -> init ()
        , update = Page.Calendar.update
        , view = \m -> { title = "Test", body = [ view m ] }
        }
        |> ProgramTest.withSimulatedEffects Tests.Effects.simulateEffects
        |> ProgramTest.start ()


pointerDownButton label =
    ProgramTest.simulateDomEvent
        (\h -> Query.findAll [ attribute (Html.Attributes.attribute "aria-label" label) ] h |> Query.first)
        (Event.custom "pointerdown" (Encode.object []))


openForm : ProgramTest Model Msg Effect -> ProgramTest Model Msg Effect
openForm program =
    program
        |> pointerDownButton "Add Activity"
        |> ensureViewHas
            [ id "activity-form"
            ]


submitForm : ProgramTest Model Msg Effect -> ProgramTest Model Msg Effect
submitForm program =
    program
        |> pointerDownButton "Save"


all : Test
all =
    describe "Activity form"
        [ test "creates new activities" <|
            \() ->
                start
                    |> openForm
                    |> fillIn "description" "Description" "Morning Run"
                    |> fillIn "minutes" "minutes" "30"
                    |> submitForm
                    |> expectViewHas
                        [ text "Morning Run"
                        , text "30m"
                        ]
        ]
