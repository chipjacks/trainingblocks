module Distance exposing (calculate, fromMeters, round1, toMeters, toRaceDistance)

import Activity.Types exposing (DistanceUnits(..), RaceDistance(..))


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


toRaceDistance : Float -> RaceDistance
toRaceDistance meters =
    let
        isEqual a b =
            abs (a - b) < toMeters Miles 0.1
    in
    if isEqual meters (toMeters Kilometers 5) then
        FiveK

    else if isEqual meters (toMeters Kilometers 8) then
        EightK

    else if isEqual meters (toMeters Miles 5) then
        FiveMile

    else if isEqual meters (toMeters Kilometers 10) then
        TenK

    else if isEqual meters (toMeters Miles 10) then
        TenMile

    else if isEqual meters (toMeters Kilometers 20) then
        TwentyK

    else if isEqual meters (toMeters Miles 13.1) then
        HalfMarathon

    else if isEqual meters (toMeters Kilometers 25) then
        TwentyFiveK

    else if isEqual meters (toMeters Kilometers 30) then
        ThirtyK

    else if isEqual meters (toMeters Miles 26.2) then
        Marathon

    else
        OtherDistance


round1 : Float -> Float
round1 float =
    float
        |> (*) 10
        |> round
        |> toFloat
        |> (\r -> r / 10)
