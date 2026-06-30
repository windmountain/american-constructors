module Format exposing (formatDays)


formatDays : Float -> String
formatDays days =
    if days == toFloat (round days) then
        String.fromInt (round days)

    else
        String.fromFloat days
