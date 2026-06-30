module FormatTest exposing (suite)

import Expect
import Format exposing (formatDays)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "formatDays"
        [ test "renders a whole number without a decimal point" <|
            \_ -> formatDays 5 |> Expect.equal "5"
        , test "renders a fractional number with its decimal point" <|
            \_ -> formatDays 0.5 |> Expect.equal "0.5"
        ]
