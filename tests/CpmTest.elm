module CpmTest exposing (suite)

import Expect
import Main exposing (Estimate(..), Item(..), Tactic(..), TaskId(..), es)
import Test exposing (Test, describe, test)


{-| A milestone (1) feeding two parallel tasks (2: 5d, 3: 8d) that both
feed into a final task (4), mirroring the diamond shape of this project's
real terrace/lobby dependencies. Task 5 carries a Range estimate to
exercise each estimate tactic.
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
    , TaskItem
        { id = TaskId 5
        , section = "test"
        , name = "range branch"
        , dependsOn = [ TaskId 1 ]
        , estimate = Range 4 10
        , weatherDependent = False
        , canExpedite = False
        }
    ]


suite : Test
suite =
    describe "es"
        [ test "a node with no dependencies can start immediately" <|
            \_ -> es Pessimistic items (TaskId 1) |> Expect.equal 0
        , test "a node waits for its single dependency to finish" <|
            \_ -> es Pessimistic items (TaskId 2) |> Expect.equal 0
        , test "a node with multiple dependencies waits for the longest one" <|
            \_ -> es Pessimistic items (TaskId 4) |> Expect.equal 8
        , test "optimistic tactic uses the low end of a dependency's range estimate" <|
            \_ -> es Optimistic (items ++ [ dependentOn5 ]) (TaskId 6) |> Expect.equal 4
        , test "pessimistic tactic uses the high end of a dependency's range estimate" <|
            \_ -> es Pessimistic (items ++ [ dependentOn5 ]) (TaskId 6) |> Expect.equal 10
        , test "midpoint tactic uses the midpoint of a dependency's range estimate" <|
            \_ -> es Midpoint (items ++ [ dependentOn5 ]) (TaskId 6) |> Expect.equal 7
        ]


dependentOn5 : Item
dependentOn5 =
    TaskItem
        { id = TaskId 6
        , section = "test"
        , name = "depends on range branch"
        , dependsOn = [ TaskId 5 ]
        , estimate = Point 0
        , weatherDependent = False
        , canExpedite = False
        }
