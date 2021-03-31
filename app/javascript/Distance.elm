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


toRaceDistance : Float -> Maybe RaceDistance
toRaceDistance meters =
    let
        isEqual a b =
            a - b < 10

        fiveK =
            toMeters Kilometers 5
    in
    if isEqual meters (toMeters Kilometers 5) then
        Just FiveK

    else if isEqual meters (toMeters Kilometers 8) then
        Just EightK

    else if isEqual meters (toMeters Miles 5) then
        Just FiveMile

    else if isEqual meters (toMeters Kilometers 10) then
        Just TenK

    else if isEqual meters (toMeters Miles 10) then
        Just TenMile

    else if isEqual meters (toMeters Kilometers 20) then
        Just TwentyK

    else if isEqual meters (toMeters Miles 13.1) then
        Just HalfMarathon

    else if isEqual meters (toMeters Kilometers 25) then
        Just TwentyFiveK

    else if isEqual meters (toMeters Kilometers 30) then
        Just ThirtyK

    else if isEqual meters (toMeters Miles 26.2) then
        Just Marathon

    else
        Nothing


round1 : Float -> Float
round1 float =
    float
        |> (*) 10
        |> round
        |> toFloat
        |> (\r -> r / 10)
