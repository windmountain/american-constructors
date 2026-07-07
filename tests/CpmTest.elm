module CpmTest exposing (suite)

import Expect
import Main exposing (Estimate(..), Item(..), Tactic(..), TaskId(..), ef, es, lf, ls, slack)
import Test exposing (Test, describe, test)


{-| A milestone (1) feeding two parallel tasks (2: 5d, 3: 8d) that both
feed into a final task (4), mirroring the diamond shape of this project's
real terrace/lobby dependencies. Task 5 carries a Range estimate to
exercise each estimate tactic.
-}
items : List Item
items =
    [ MilestoneItem
        { id = TaskId "1"
        , section = "test"
        , name = "start"
        , dependsOn = []
        , isEffectiveEnd = False
        , weatherDependent = False
        , canExpedite = False
        , spreadsheetEs = 0
        , spreadsheetEf = 0
        , spreadsheetLf = 0
        , spreadsheetLs = 0
        , spreadsheetSlack = 0
        }
    , TaskItem
        { id = TaskId "2"
        , section = "test"
        , name = "short branch"
        , dependsOn = [ TaskId "1" ]
        , estimate = Point 5
        , isEffectiveEnd = False
        , weatherDependent = False
        , canExpedite = False
        , spreadsheetEs = 0
        , spreadsheetEf = 0
        , spreadsheetLf = 0
        , spreadsheetLs = 0
        , spreadsheetSlack = 0
        }
    , TaskItem
        { id = TaskId "3"
        , section = "test"
        , name = "long branch"
        , dependsOn = [ TaskId "1" ]
        , estimate = Point 8
        , isEffectiveEnd = False
        , weatherDependent = False
        , canExpedite = False
        , spreadsheetEs = 0
        , spreadsheetEf = 0
        , spreadsheetLf = 0
        , spreadsheetLs = 0
        , spreadsheetSlack = 0
        }
    , TaskItem
        { id = TaskId "4"
        , section = "test"
        , name = "join"
        , dependsOn = [ TaskId "2", TaskId "3" ]
        , estimate = Point 2
        , isEffectiveEnd = True
        , weatherDependent = False
        , canExpedite = False
        , spreadsheetEs = 0
        , spreadsheetEf = 0
        , spreadsheetLf = 0
        , spreadsheetLs = 0
        , spreadsheetSlack = 0
        }
    , TaskItem
        { id = TaskId "5"
        , section = "test"
        , name = "range branch"
        , dependsOn = [ TaskId "1" ]
        , estimate = Range 4 10
        , isEffectiveEnd = False
        , weatherDependent = False
        , canExpedite = False
        , spreadsheetEs = 0
        , spreadsheetEf = 0
        , spreadsheetLf = 0
        , spreadsheetLs = 0
        , spreadsheetSlack = 0
        }
    ]


suite : Test
suite =
    describe "cpm"
        [ describe "es"
            [ test "a node with no dependencies can start immediately" <|
                \_ -> es Pessimistic items (TaskId "1") |> Expect.equal 0
            , test "a node waits for its single dependency to finish" <|
                \_ -> es Pessimistic items (TaskId "2") |> Expect.equal 0
            , test "a node with multiple dependencies waits for the longest one" <|
                \_ -> es Pessimistic items (TaskId "4") |> Expect.equal 8
            , test "optimistic tactic uses the low end of a dependency's range estimate" <|
                \_ -> es Optimistic (items ++ [ dependentOn5 ]) (TaskId "6") |> Expect.equal 4
            , test "pessimistic tactic uses the high end of a dependency's range estimate" <|
                \_ -> es Pessimistic (items ++ [ dependentOn5 ]) (TaskId "6") |> Expect.equal 10
            , test "midpoint tactic uses the midpoint of a dependency's range estimate" <|
                \_ -> es Midpoint (items ++ [ dependentOn5 ]) (TaskId "6") |> Expect.equal 7
            ]
        , describe "ef"
            [ test "a task's EF is its ES plus its own duration" <|
                \_ -> ef Pessimistic items (TaskId "2") |> Expect.equal 5
            , test "a join task's EF adds its duration to the longest dependency's EF" <|
                \_ -> ef Pessimistic items (TaskId "4") |> Expect.equal 10
            ]
        , describe "lf"
            [ test "the effective end item finishes as late as the project finish" <|
                \_ -> lf Pessimistic items (TaskId "4") |> Expect.equal (Just 10)
            , test "a task's LF is the earliest LS among its dependents" <|
                \_ -> lf Pessimistic items (TaskId "2") |> Expect.equal (Just 8)
            , test "a task scheduled after the effective end item has no LF" <|
                \_ -> lf Pessimistic (items ++ [ dependentOnEnd ]) (TaskId "7") |> Expect.equal Nothing
            ]
        , describe "ls"
            [ test "a task's LS is its LF minus its own duration" <|
                \_ -> ls Pessimistic items (TaskId "2") |> Expect.equal (Just 3)
            , test "a task scheduled after the effective end item has no LS" <|
                \_ -> ls Pessimistic (items ++ [ dependentOnEnd ]) (TaskId "7") |> Expect.equal Nothing
            ]
        , describe "slack"
            [ test "a critical path task has zero slack" <|
                \_ -> slack Pessimistic items (TaskId "3") |> Expect.equal (Just 0)
            , test "a task off the critical path has positive slack" <|
                \_ -> slack Pessimistic items (TaskId "2") |> Expect.equal (Just 3)
            , test "a task scheduled after the effective end item has no slack" <|
                \_ -> slack Pessimistic (items ++ [ dependentOnEnd ]) (TaskId "7") |> Expect.equal Nothing
            , test "a task scheduled after the effective end item still has an ES/EF" <|
                \_ ->
                    ( es Pessimistic (items ++ [ dependentOnEnd ]) (TaskId "7")
                    , ef Pessimistic (items ++ [ dependentOnEnd ]) (TaskId "7")
                    )
                        |> Expect.equal ( 10, 13 )
            ]
        ]


dependentOn5 : Item
dependentOn5 =
    TaskItem
        { id = TaskId "6"
        , section = "test"
        , name = "depends on range branch"
        , dependsOn = [ TaskId "5" ]
        , estimate = Point 0
        , isEffectiveEnd = False
        , weatherDependent = False
        , canExpedite = False
        , spreadsheetEs = 0
        , spreadsheetEf = 0
        , spreadsheetLf = 0
        , spreadsheetLs = 0
        , spreadsheetSlack = 0
        }


{-| Scheduled after the effective end item (4), so it's not one of its
ancestors - it should keep its ES/EF but have no LS/LF/slack.
-}
dependentOnEnd : Item
dependentOnEnd =
    TaskItem
        { id = TaskId "7"
        , section = "test"
        , name = "depends on join"
        , dependsOn = [ TaskId "4" ]
        , estimate = Point 3
        , isEffectiveEnd = False
        , weatherDependent = False
        , canExpedite = False
        , spreadsheetEs = 0
        , spreadsheetEf = 0
        , spreadsheetLf = 0
        , spreadsheetLs = 0
        , spreadsheetSlack = 0
        }
