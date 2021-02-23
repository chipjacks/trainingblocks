module Tests.ActivityForm exposing (all)

import Effect exposing (Effect)
import Main
import Msg exposing (Msg)
import ProgramTest exposing (..)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (class, id, tag, text)
import Tests.Effects


start : ProgramTest Main.Model Msg Effect
start =
    ProgramTest.createDocument
        { init = \_ -> Main.init { csrfToken = "asdf" }
        , update = Main.update
        , view = \m -> { title = "Test", body = [ Main.view m ] }
        }
        |> ProgramTest.withSimulatedEffects Tests.Effects.simulateEffects
        |> ProgramTest.start ()


openForm : ProgramTest Main.Model Msg Effect -> ProgramTest Main.Model Msg Effect
openForm program =
    program
        |> ProgramTest.simulateHttpOk
            "GET"
            "activities"
            """{"rev":"K001", "activities":[]}"""
        |> ProgramTest.within
            (\h -> Query.findAll [ class "add-button" ] h |> Query.first)
            (clickButton "Add")
        |> ensureViewHas
            [ id "activity-form"
            ]


submitForm : ProgramTest Main.Model Msg Effect -> ProgramTest Main.Model Msg Effect
submitForm program =
    program
        |> clickButton "Save"


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
