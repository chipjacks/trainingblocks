module Distance exposing (calculate, fromMeters, round1, toMeters)

import Activity.Types exposing (DistanceUnits(..))


calculate : DistanceUnits -> Int -> Int -> Float
calculate units duration pace =
    toFloat duration
        / toFloat pace
        |> toMeters Miles
        |> fromMeters units
        |> round1


toMeters : DistanceUnits -> Float -> Float
toMeters units dist =
    dist * distanceConversion units


fromMeters : DistanceUnits -> Float -> Float
fromMeters units meters =
    meters
        / distanceConversion units


distanceConversion : DistanceUnits -> Float
distanceConversion units =
    case units of
        Miles ->
            1609.344

        Kilometers ->
            1000

        Meters ->
            1

        Yards ->
            0.9144


round1 : Float -> Float
round1 float =
    float
        |> (*) 10
        |> round
        |> toFloat
        |> (\r -> r / 10)
