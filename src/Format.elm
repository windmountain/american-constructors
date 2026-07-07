module Format exposing (formatDays, formatMaybeDays)


formatDays : Float -> String
formatDays days =
    if days == toFloat (round days) then
        String.fromInt (round days)

    else
        String.fromFloat days


formatMaybeDays : Maybe Float -> String
formatMaybeDays maybeDays =
    maybeDays |> Maybe.map formatDays |> Maybe.withDefault ""
