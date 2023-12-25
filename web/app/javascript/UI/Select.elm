module UI.Select exposing (select, view, withAttributes)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode


type alias Config msg =
    { onChange : String -> msg
    , options : List String
    , attrs : List (Html.Attribute msg)
    }


select : (String -> msg) -> List String -> Config msg
select onChange options =
    { onChange = onChange
    , options = options
    , attrs = []
    }


withAttributes : List (Html.Attribute msg) -> Config msg -> Config msg
withAttributes attrs config =
    { config | attrs = config.attrs ++ attrs }


view : String -> Config msg -> Html msg
view currentValue config =
    let
        eventDecoder =
            Decode.at [ "target", "value" ] Decode.string
                |> Decode.map config.onChange
    in
    Html.select
        (Html.Events.on "change" eventDecoder
            :: config.attrs
        )
        (List.map
            (\str ->
                Html.option
                    [ Html.Attributes.value str
                    , Html.Attributes.selected (str == currentValue)
                    ]
                    [ Html.text str ]
            )
            config.options
        )
