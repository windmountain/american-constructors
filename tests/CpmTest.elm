module CpmTest exposing (suite)

import Expect
import Main exposing (Estimate(..), Item(..), TaskId(..), es)
import Test exposing (Test, describe, test)


{-| A milestone (1) feeding two parallel tasks (2: 5d, 3: 3d) that both
feed into a final task (4), mirroring the diamond shape of this project's
real terrace/lobby dependencies.
-}
items : List Item
items =
    [ MilestoneItem
        { id = TaskId 1
        , section = "test"
        , name = "start"
        , dependsOn = []
        , weatherDependent = False
        , canExpedite = False
        }
    , TaskItem
        { id = TaskId 2
        , section = "test"
        , name = "short branch"
        , dependsOn = [ TaskId 1 ]
        , estimate = Point 5
        , weatherDependent = False
        , canExpedite = False
        }
    , TaskItem
        { id = TaskId 3
        , section = "test"
        , name = "long branch"
        , dependsOn = [ TaskId 1 ]
        , estimate = Point 8
        , weatherDependent = False
        , canExpedite = False
        }
    , TaskItem
        { id = TaskId 4
        , section = "test"
        , name = "join"
        , dependsOn = [ TaskId 2, TaskId 3 ]
        , estimate = Point 2
        , weatherDependent = False
        , canExpedite = False
        }
    ]


suite : Test
suite =
    describe "es"
        [ test "a node with no dependencies can start immediately" <|
            \_ -> es items (TaskId 1) |> Expect.equal 0
        , test "a node waits for its single dependency to finish" <|
            \_ -> es items (TaskId 2) |> Expect.equal 0
        , test "a node with multiple dependencies waits for the longest one" <|
            \_ -> es items (TaskId 4) |> Expect.equal 8
        ]
