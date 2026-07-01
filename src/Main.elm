module Main exposing (Estimate(..), Item(..), Milestone, Tactic(..), Task, TaskId(..), es, ls, main, slack)

import Browser
import Csv.Decode as Decode exposing (Decoder)
import Data
import Dict exposing (Dict)
import Format
import Html exposing (Html, div, label, node, option, pre, select, text)
import Html.Attributes exposing (for, id, property, selected, value)
import Html.Events exposing (onInput)
import Json.Encode as Encode


type TaskId
    = TaskId Int


type Tactic
    = Optimistic
    | Pessimistic
    | Midpoint


type Estimate
    = Point Float
    | Range Float Float


type alias Task =
    { id : TaskId
    , section : String
    , name : String
    , dependsOn : List TaskId
    , estimate : Estimate
    , weatherDependent : Bool
    , canExpedite : Bool
    }


type alias Milestone =
    { id : TaskId
    , section : String
    , name : String
    , dependsOn : List TaskId
    , weatherDependent : Bool
    , canExpedite : Bool
    }


type Item
    = TaskItem Task
    | MilestoneItem Milestone


type alias RawFields =
    { id : TaskId
    , section : String
    , name : String
    , dependsOn : List TaskId
    , estimate : Maybe Estimate
    , weatherDependent : Bool
    , canExpedite : Bool
    }


itemDecoder : Decoder Item
itemDecoder =
    Decode.into RawFields
        |> Decode.pipeline (Decode.field "Id" (Decode.map TaskId Decode.int))
        |> Decode.pipeline (Decode.field "Section" Decode.string)
        |> Decode.pipeline (Decode.field "Name" Decode.string)
        |> Decode.pipeline dependsOnDecoder
        |> Decode.pipeline estimateDecoder
        |> Decode.pipeline (yesNoDecoder "Weather-dependent")
        |> Decode.pipeline (yesNoDecoder "Can Expedite")
        |> Decode.map toItem


toItem : RawFields -> Item
toItem fields =
    case fields.estimate of
        Just estimate ->
            TaskItem
                { id = fields.id
                , section = fields.section
                , name = fields.name
                , dependsOn = fields.dependsOn
                , estimate = estimate
                , weatherDependent = fields.weatherDependent
                , canExpedite = fields.canExpedite
                }

        Nothing ->
            MilestoneItem
                { id = fields.id
                , section = fields.section
                , name = fields.name
                , dependsOn = fields.dependsOn
                , weatherDependent = fields.weatherDependent
                , canExpedite = fields.canExpedite
                }


dependsOnDecoder : Decoder (List TaskId)
dependsOnDecoder =
    Decode.into (\a b c d -> List.filterMap identity [ a, b, c, d ])
        |> Decode.pipeline (optionalIntField "Deps on (1)")
        |> Decode.pipeline (optionalIntField "Deps on (2)")
        |> Decode.pipeline (optionalIntField "Deps on (3)")
        |> Decode.pipeline (optionalIntField "Deps on (4)")
        |> Decode.map (List.map TaskId)


optionalIntField : String -> Decoder (Maybe Int)
optionalIntField name =
    Decode.field name Decode.string
        |> Decode.map String.trim
        |> Decode.andThen
            (\value ->
                if value == "" then
                    Decode.succeed Nothing

                else
                    case String.toInt value of
                        Just n ->
                            Decode.succeed (Just n)

                        Nothing ->
                            Decode.fail ("Could not parse \"" ++ value ++ "\" as an int in field " ++ name)
            )


optionalFloatField : String -> Decoder (Maybe Float)
optionalFloatField name =
    Decode.field name Decode.string
        |> Decode.map String.trim
        |> Decode.andThen
            (\value ->
                if value == "" then
                    Decode.succeed Nothing

                else
                    case String.toFloat value of
                        Just n ->
                            Decode.succeed (Just n)

                        Nothing ->
                            Decode.fail ("Could not parse \"" ++ value ++ "\" as a number in field " ++ name)
            )


estimateDecoder : Decoder (Maybe Estimate)
estimateDecoder =
    Decode.map3 (\point low high -> ( point, low, high ))
        (optionalFloatField "Estimate")
        (optionalFloatField "Low Estimate")
        (optionalFloatField "High Estimate")
        |> Decode.andThen
            (\triple ->
                case triple of
                    ( Nothing, Nothing, Nothing ) ->
                        Decode.succeed Nothing

                    ( Just point, Nothing, Nothing ) ->
                        Decode.succeed (Just (Point point))

                    ( Nothing, Just low, Just high ) ->
                        Decode.succeed (Just (Range low high))

                    other ->
                        Decode.fail ("Expected either an Estimate, a Low/High Estimate pair, or neither, got " ++ Debug.toString other)
            )


yesNoDecoder : String -> Decoder Bool
yesNoDecoder name =
    Decode.field name Decode.string
        |> Decode.map (\value -> String.trim value == "yes")


itemsResult : Result Decode.Error (List Item)
itemsResult =
    Decode.decodeCsv Decode.FieldNamesFromFirstRow itemDecoder Data.csvData


type alias Model =
    { tactic : Tactic }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tactic = Midpoint }, Cmd.none )


type Msg
    = SetTactic Tactic


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTactic tactic ->
            ( { model | tactic = tactic }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "AC Tasks"
    , body =
        [ div []
            [ tacticSelect model.tactic
            , case itemsResult of
                Ok items ->
                    viewGraph model.tactic items

                Err error ->
                    pre [] [ text (Decode.errorToString error) ]
            ]
        ]
    }


tacticSelect : Tactic -> Html Msg
tacticSelect current =
    div []
        [ label [ for "tactic-select" ] [ text "ranged estimate selection: " ]
        , select [ id "tactic-select", onInput (tacticFromString >> Maybe.withDefault current >> SetTactic) ]
            [ option [ value "optimistic", selected (current == Optimistic) ] [ text "Optimistic" ]
            , option [ value "pessimistic", selected (current == Pessimistic) ] [ text "Pessimistic" ]
            , option [ value "midpoint", selected (current == Midpoint) ] [ text "Midpoint" ]
            ]
        ]


tacticFromString : String -> Maybe Tactic
tacticFromString value_ =
    case value_ of
        "optimistic" ->
            Just Optimistic

        "pessimistic" ->
            Just Pessimistic

        "midpoint" ->
            Just Midpoint

        _ ->
            Nothing


viewGraph : Tactic -> List Item -> Html Msg
viewGraph tactic items =
    node "cytoscape-graph" [ property "elements" (encodeElements (buildSchedule tactic items) items) ] []


encodeElements : Schedule -> List Item -> Encode.Value
encodeElements schedule items =
    Encode.list identity (List.concatMap (itemToElements schedule) items)


itemFields : Schedule -> Item -> { id : TaskId, label : String, dependsOn : List TaskId, kind : String }
itemFields schedule item =
    case item of
        TaskItem task ->
            { id = task.id
            , label = String.join "\n" [ "[" ++ task.section ++ "]", task.name ++ " (" ++ estimateText task.estimate ++ ")", scheduleText schedule task.id ]
            , dependsOn = task.dependsOn
            , kind = "task"
            }

        MilestoneItem milestone ->
            { id = milestone.id
            , label = String.join "\n" [ "[" ++ milestone.section ++ "]", milestone.name, scheduleText schedule milestone.id ]
            , dependsOn = milestone.dependsOn
            , kind = "milestone"
            }


scheduleText : Schedule -> TaskId -> String
scheduleText schedule taskId =
    "ES "
        ++ Format.formatDays (scheduleEs schedule taskId)
        ++ "d"
        ++ "  LS "
        ++ Format.formatDays (scheduleLs schedule taskId)
        ++ "d"
        ++ "  Slack "
        ++ Format.formatDays (scheduleSlack schedule taskId)
        ++ "d"


estimateText : Estimate -> String
estimateText estimate =
    case estimate of
        Point days ->
            Format.formatDays days ++ "d"

        Range low high ->
            Format.formatDays low ++ "-" ++ Format.formatDays high ++ "d"


{-| Earliest/latest start times for every item, computed once per (tactic, items)
via a single forward and backward pass over a topologically sorted dependency
graph, rather than by re-walking the dependency tree from scratch for every
lookup.
-}
type alias Schedule =
    { es : Dict Int Float
    , ls : Dict Int Float
    }


buildSchedule : Tactic -> List Item -> Schedule
buildSchedule tactic items =
    let
        itemsById : Dict Int Item
        itemsById =
            items
                |> List.map (\item -> ( taskIdToInt (itemId item), item ))
                |> Dict.fromList

        dependentsOf : Dict Int (List Int)
        dependentsOf =
            items
                |> List.concatMap
                    (\item ->
                        itemDependsOn item
                            |> List.map (\dep -> ( taskIdToInt dep, taskIdToInt (itemId item) ))
                    )
                |> List.foldl
                    (\( depId, dependentId ) acc ->
                        Dict.update depId (\existing -> Just (dependentId :: Maybe.withDefault [] existing)) acc
                    )
                    Dict.empty

        topoOrder : List Int
        topoOrder =
            topoSort itemsById dependentsOf

        durationOf : Int -> Float
        durationOf id =
            case Dict.get id itemsById of
                Just (TaskItem task) ->
                    estimateDuration tactic task.estimate

                Just (MilestoneItem _) ->
                    0

                Nothing ->
                    0

        esDict : Dict Int Float
        esDict =
            List.foldl
                (\id acc ->
                    let
                        deps =
                            Dict.get id itemsById
                                |> Maybe.map itemDependsOn
                                |> Maybe.withDefault []

                        esValue =
                            deps
                                |> List.map
                                    (\depId ->
                                        let
                                            d =
                                                taskIdToInt depId
                                        in
                                        (Dict.get d acc |> Maybe.withDefault 0) + durationOf d
                                    )
                                |> List.maximum
                                |> Maybe.withDefault 0
                    in
                    Dict.insert id esValue acc
                )
                Dict.empty
                topoOrder

        finish : Float
        finish =
            itemsById
                |> Dict.keys
                |> List.map (\id -> (Dict.get id esDict |> Maybe.withDefault 0) + durationOf id)
                |> List.maximum
                |> Maybe.withDefault 0

        lsDict : Dict Int Float
        lsDict =
            List.foldl
                (\id acc ->
                    let
                        dependents =
                            Dict.get id dependentsOf |> Maybe.withDefault []

                        lf =
                            case dependents of
                                [] ->
                                    finish

                                _ ->
                                    dependents
                                        |> List.map (\d -> Dict.get d acc |> Maybe.withDefault finish)
                                        |> List.minimum
                                        |> Maybe.withDefault finish
                    in
                    Dict.insert id (lf - durationOf id) acc
                )
                Dict.empty
                (List.reverse topoOrder)
    in
    { es = esDict, ls = lsDict }


{-| Kahn's algorithm: repeatedly peel off items with no unprocessed
dependencies so each item is visited exactly once.
-}
topoSort : Dict Int Item -> Dict Int (List Int) -> List Int
topoSort itemsById dependentsOf =
    let
        inDegree0 : Dict Int Int
        inDegree0 =
            itemsById |> Dict.map (\_ item -> List.length (itemDependsOn item))

        initialQueue : List Int
        initialQueue =
            inDegree0 |> Dict.filter (\_ deg -> deg == 0) |> Dict.keys
    in
    topoSortHelp dependentsOf inDegree0 initialQueue []


topoSortHelp : Dict Int (List Int) -> Dict Int Int -> List Int -> List Int -> List Int
topoSortHelp dependentsOf inDegree queue order =
    case queue of
        [] ->
            List.reverse order

        id :: rest ->
            let
                dependents =
                    Dict.get id dependentsOf |> Maybe.withDefault []

                ( newInDegree, newlyReady ) =
                    List.foldl
                        (\dep ( degAcc, readyAcc ) ->
                            let
                                updated =
                                    (Dict.get dep degAcc |> Maybe.withDefault 1) - 1
                            in
                            ( Dict.insert dep updated degAcc
                            , if updated == 0 then
                                dep :: readyAcc

                              else
                                readyAcc
                            )
                        )
                        ( inDegree, [] )
                        dependents
            in
            topoSortHelp dependentsOf newInDegree (rest ++ newlyReady) (id :: order)


scheduleEs : Schedule -> TaskId -> Float
scheduleEs schedule taskId =
    Dict.get (taskIdToInt taskId) schedule.es |> Maybe.withDefault 0


scheduleLs : Schedule -> TaskId -> Float
scheduleLs schedule taskId =
    Dict.get (taskIdToInt taskId) schedule.ls |> Maybe.withDefault 0


scheduleSlack : Schedule -> TaskId -> Float
scheduleSlack schedule taskId =
    scheduleLs schedule taskId - scheduleEs schedule taskId


es : Tactic -> List Item -> TaskId -> Float
es tactic items taskId =
    scheduleEs (buildSchedule tactic items) taskId


ls : Tactic -> List Item -> TaskId -> Float
ls tactic items taskId =
    scheduleLs (buildSchedule tactic items) taskId


slack : Tactic -> List Item -> TaskId -> Float
slack tactic items taskId =
    scheduleSlack (buildSchedule tactic items) taskId


estimateDuration : Tactic -> Estimate -> Float
estimateDuration tactic estimate =
    case estimate of
        Point days ->
            days

        Range low high ->
            case tactic of
                Optimistic ->
                    low

                Pessimistic ->
                    high

                Midpoint ->
                    (low + high) / 2


taskIdToInt : TaskId -> Int
taskIdToInt (TaskId id) =
    id


itemId : Item -> TaskId
itemId item =
    case item of
        TaskItem task ->
            task.id

        MilestoneItem milestone ->
            milestone.id


itemDependsOn : Item -> List TaskId
itemDependsOn item =
    case item of
        TaskItem task ->
            task.dependsOn

        MilestoneItem milestone ->
            milestone.dependsOn


itemToElements : Schedule -> Item -> List Encode.Value
itemToElements schedule item =
    let
        fields =
            itemFields schedule item

        nodeElement =
            Encode.object
                [ ( "data"
                  , Encode.object
                        [ ( "id", encodeTaskId fields.id )
                        , ( "label", Encode.string fields.label )
                        , ( "kind", Encode.string fields.kind )
                        , ( "slack", Encode.float (scheduleSlack schedule fields.id) )
                        ]
                  )
                ]
    in
    nodeElement :: List.map (edgeElement fields.id) fields.dependsOn


edgeElement : TaskId -> TaskId -> Encode.Value
edgeElement dependent dependency =
    Encode.object
        [ ( "data"
          , Encode.object
                [ ( "id", Encode.string (taskIdToString dependency ++ "->" ++ taskIdToString dependent) )
                , ( "source", encodeTaskId dependency )
                , ( "target", encodeTaskId dependent )
                ]
          )
        ]


encodeTaskId : TaskId -> Encode.Value
encodeTaskId id =
    Encode.string (taskIdToString id)


taskIdToString : TaskId -> String
taskIdToString (TaskId id) =
    String.fromInt id


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
