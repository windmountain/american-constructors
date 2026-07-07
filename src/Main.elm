module Main exposing (Estimate(..), Item(..), Milestone, Tactic(..), Task, TaskId(..), ef, es, lf, ls, main, slack)

import Browser
import Csv.Decode as Decode exposing (Decoder)
import Data
import Dict exposing (Dict)
import Format
import Html exposing (Html, a, button, div, input, label, node, option, pre, select, text)
import Html.Attributes exposing (checked, download, for, href, id, name, property, selected, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode


type TaskId
    = TaskId String


type Tactic
    = Optimistic
    | Pessimistic
    | Midpoint


type ViewMode
    = NetworkView
    | CalendarView


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
    , spreadsheetEs : Float
    , spreadsheetEf : Float
    , spreadsheetLf : Float
    , spreadsheetLs : Float
    , spreadsheetSlack : Float
    }


type alias Milestone =
    { id : TaskId
    , section : String
    , name : String
    , dependsOn : List TaskId
    , weatherDependent : Bool
    , canExpedite : Bool
    , spreadsheetEs : Float
    , spreadsheetEf : Float
    , spreadsheetLf : Float
    , spreadsheetLs : Float
    , spreadsheetSlack : Float
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
    , spreadsheetEs : Float
    , spreadsheetEf : Float
    , spreadsheetLf : Float
    , spreadsheetLs : Float
    , spreadsheetSlack : Float
    }


itemDecoder : Decoder Item
itemDecoder =
    Decode.into RawFields
        |> Decode.pipeline (Decode.field "Id" (Decode.map (String.trim >> TaskId) Decode.string))
        |> Decode.pipeline (Decode.field "Section" Decode.string)
        |> Decode.pipeline (Decode.field "Name" Decode.string)
        |> Decode.pipeline dependsOnDecoder
        |> Decode.pipeline estimateDecoder
        |> Decode.pipeline (yesNoDecoder "Weather-dependent")
        |> Decode.pipeline (yesNoDecoder "Can Expedite")
        |> Decode.pipeline (requiredFloatField "ES")
        |> Decode.pipeline (requiredFloatField "EF")
        |> Decode.pipeline (requiredFloatField "LF")
        |> Decode.pipeline (requiredFloatField "LS")
        |> Decode.pipeline (requiredFloatField "Slack")
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
                , spreadsheetEs = fields.spreadsheetEs
                , spreadsheetEf = fields.spreadsheetEf
                , spreadsheetLf = fields.spreadsheetLf
                , spreadsheetLs = fields.spreadsheetLs
                , spreadsheetSlack = fields.spreadsheetSlack
                }

        Nothing ->
            MilestoneItem
                { id = fields.id
                , section = fields.section
                , name = fields.name
                , dependsOn = fields.dependsOn
                , weatherDependent = fields.weatherDependent
                , canExpedite = fields.canExpedite
                , spreadsheetEs = fields.spreadsheetEs
                , spreadsheetEf = fields.spreadsheetEf
                , spreadsheetLf = fields.spreadsheetLf
                , spreadsheetLs = fields.spreadsheetLs
                , spreadsheetSlack = fields.spreadsheetSlack
                }


dependsOnDecoder : Decoder (List TaskId)
dependsOnDecoder =
    Decode.into (\a b c d -> List.filterMap identity [ a, b, c, d ])
        |> Decode.pipeline (optionalStringField "Deps on (1)")
        |> Decode.pipeline (optionalStringField "Deps on (2)")
        |> Decode.pipeline (optionalStringField "Deps on (3)")
        |> Decode.pipeline (optionalStringField "Deps on (4)")
        |> Decode.map (List.map TaskId)


optionalStringField : String -> Decoder (Maybe String)
optionalStringField name =
    Decode.field name Decode.string
        |> Decode.map String.trim
        |> Decode.andThen
            (\value ->
                if value == "" then
                    Decode.succeed Nothing

                else
                    Decode.succeed (Just value)
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


requiredFloatField : String -> Decoder Float
requiredFloatField name =
    Decode.field name Decode.string
        |> Decode.map String.trim
        |> Decode.andThen
            (\value ->
                case String.toFloat value of
                    Just n ->
                        Decode.succeed n

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
    { tactic : Tactic
    , showSpreadsheet : Bool
    , viewMode : ViewMode
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tactic = Midpoint, showSpreadsheet = False, viewMode = NetworkView }, Cmd.none )


type Msg
    = SetTactic Tactic
    | ToggleShowSpreadsheet
    | SetViewMode ViewMode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTactic tactic ->
            ( { model | tactic = tactic }, Cmd.none )

        ToggleShowSpreadsheet ->
            ( { model | showSpreadsheet = not model.showSpreadsheet }, Cmd.none )

        SetViewMode viewMode ->
            ( { model | viewMode = viewMode }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    let
        {- The spreadsheet's ES/EF/LF/LS/Slack columns are fixed values computed
           under the midpoint tactic, so comparing against them only makes sense
           when midpoint is selected.
        -}
        showSpreadsheet : Bool
        showSpreadsheet =
            model.showSpreadsheet && model.tactic == Midpoint
    in
    { title = "AC Tasks"
    , body =
        [ toolbar model.tactic model.viewMode
        , case itemsResult of
            Ok items ->
                viewMain model.viewMode model.tactic showSpreadsheet items

            Err error ->
                pre [] [ text (Decode.errorToString error) ]
        , viewDownloads
        , if model.tactic == Midpoint then
            viewSpreadsheetToggle model.showSpreadsheet

          else
            text ""
        ]
    }


viewMain : ViewMode -> Tactic -> Bool -> List Item -> Html Msg
viewMain viewMode tactic showSpreadsheet items =
    case viewMode of
        NetworkView ->
            viewGraph tactic showSpreadsheet items

        CalendarView ->
            viewCalendarPlaceholder


{-| Standing in for a future calendar view (FeatureIdeas.md item 7); for now
this just proves the view toggle switches content.
-}
viewCalendarPlaceholder : Html msg
viewCalendarPlaceholder =
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "height" "80vh"
        , style "color" "#6b7280"
        , style "font-family" "-apple-system, BlinkMacSystemFont, sans-serif"
        ]
        [ text "Calendar view coming soon" ]


{-| Links to the underlying spreadsheet in its various forms. These are
static files sitting alongside index.html (checked into the repo, kept in
sync with the ODS by the pregenerate build step), not anything Elm
generates, so plain download links are all that's needed.
-}
viewDownloads : Html msg
viewDownloads =
    div
        [ style "position" "fixed"
        , style "bottom" "16px"
        , style "right" "16px"
        , style "display" "flex"
        , style "gap" "8px"
        , style "align-items" "center"
        , style "background" "rgba(255, 255, 255, 0.9)"
        , style "border" "1px solid #9ca3af"
        , style "border-radius" "6px"
        , style "padding" "8px 12px"
        , style "font-size" "12px"
        , style "font-family" "-apple-system, BlinkMacSystemFont, sans-serif"
        , style "z-index" "20"
        ]
        [ text "Download:"
        , downloadLink "AC%20Tasks.xlsx" "XLSX"
        , downloadLink "AC%20Tasks.csv" "CSV"
        , downloadLink "AC%20Tasks.ods" "ODS"
        ]


downloadLink : String -> String -> Html msg
downloadLink file label_ =
    a [ href file, download "" ] [ text label_ ]


viewSpreadsheetToggle : Bool -> Html Msg
viewSpreadsheetToggle showSpreadsheet =
    div
        [ style "position" "fixed"
        , style "bottom" "16px"
        , style "left" "16px"
        , style "z-index" "20"
        ]
        [ button [ onClick ToggleShowSpreadsheet ]
            [ text
                (if showSpreadsheet then
                    "Hide spreadsheet calculations"

                 else
                    "Show spreadsheet calculations"
                )
            ]
        ]


{-| Left: tactic select. Center: network/calendar view toggle. The trailing
empty div balances the leading tactic select in the 1fr/auto/1fr grid so the
center column stays centered regardless of how wide the other two are.
-}
toolbar : Tactic -> ViewMode -> Html Msg
toolbar tactic viewMode =
    div
        [ style "display" "grid"
        , style "grid-template-columns" "1fr auto 1fr"
        , style "align-items" "center"
        , style "padding" "12px 16px"
        , style "font-family" "-apple-system, BlinkMacSystemFont, sans-serif"
        , style "font-size" "12px"
        ]
        [ tacticSelect tactic
        , viewModeToggle viewMode
        , div [] []
        ]


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


viewModeToggle : ViewMode -> Html Msg
viewModeToggle current =
    div
        [ style "display" "flex"
        , style "gap" "16px"
        , style "justify-self" "center"
        ]
        [ viewModeRadio "Network" NetworkView current
        , viewModeRadio "Calendar" CalendarView current
        ]


viewModeRadio : String -> ViewMode -> ViewMode -> Html Msg
viewModeRadio label_ value_ current =
    label
        [ style "display" "flex"
        , style "align-items" "center"
        , style "gap" "4px"
        , style "cursor" "pointer"
        ]
        [ input
            [ type_ "radio"
            , name "view-mode"
            , checked (value_ == current)
            , onClick (SetViewMode value_)
            ]
            []
        , text label_
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


viewGraph : Tactic -> Bool -> List Item -> Html Msg
viewGraph tactic showSpreadsheet items =
    node "cytoscape-graph" [ property "elements" (encodeElements (buildSchedule tactic items) showSpreadsheet items) ] []


encodeElements : Schedule -> Bool -> List Item -> Encode.Value
encodeElements schedule showSpreadsheet items =
    Encode.list identity (List.concatMap (itemToElements schedule showSpreadsheet) items)


itemFields : Schedule -> Bool -> Item -> { id : TaskId, label : String, dependsOn : List TaskId, kind : String, card : List ( String, Encode.Value ) }
itemFields schedule showSpreadsheet item =
    case item of
        TaskItem task ->
            { id = task.id
            , label = itemLabel schedule task.id [ "[" ++ task.section ++ "]", task.name ++ " (" ++ estimateText task.estimate ++ ")" ]
            , dependsOn = task.dependsOn
            , kind = "task"
            , card =
                [ ( "name", Encode.string task.name )
                , ( "section", Encode.string task.section )
                , ( "estimate", Encode.string (estimateText task.estimate) )
                , ( "es", Encode.string (Format.formatDays (scheduleEs schedule task.id)) )
                , ( "ef", Encode.string (Format.formatDays (scheduleEf schedule task.id)) )
                , ( "lf", Encode.string (Format.formatDays (scheduleLf schedule task.id)) )
                , ( "ls", Encode.string (Format.formatDays (scheduleLs schedule task.id)) )
                , ( "sEs", Encode.string (Format.formatDays task.spreadsheetEs) )
                , ( "sEf", Encode.string (Format.formatDays task.spreadsheetEf) )
                , ( "sLf", Encode.string (Format.formatDays task.spreadsheetLf) )
                , ( "sLs", Encode.string (Format.formatDays task.spreadsheetLs) )
                , ( "showSpreadsheet", Encode.bool showSpreadsheet )
                ]
            }

        MilestoneItem milestone ->
            { id = milestone.id
            , label = itemLabel schedule milestone.id [ "[" ++ milestone.section ++ "]", milestone.name ]
            , dependsOn = milestone.dependsOn
            , kind = "milestone"
            , card =
                [ ( "name", Encode.string milestone.name )
                , ( "section", Encode.string milestone.section )
                , ( "es", Encode.string (Format.formatDays (scheduleEs schedule milestone.id)) )
                , ( "ef", Encode.string (Format.formatDays (scheduleEf schedule milestone.id)) )
                , ( "lf", Encode.string (Format.formatDays (scheduleLf schedule milestone.id)) )
                , ( "ls", Encode.string (Format.formatDays (scheduleLs schedule milestone.id)) )
                , ( "sEs", Encode.string (Format.formatDays milestone.spreadsheetEs) )
                , ( "sEf", Encode.string (Format.formatDays milestone.spreadsheetEf) )
                , ( "sLf", Encode.string (Format.formatDays milestone.spreadsheetLf) )
                , ( "sLs", Encode.string (Format.formatDays milestone.spreadsheetLs) )
                , ( "showSpreadsheet", Encode.bool showSpreadsheet )
                ]
            }


itemLabel : Schedule -> TaskId -> List String -> String
itemLabel schedule taskId headerLines =
    String.join "\n" (headerLines ++ [ scheduleText schedule taskId ])


scheduleText : Schedule -> TaskId -> String
scheduleText schedule taskId =
    "ES "
        ++ Format.formatDays (scheduleEs schedule taskId)
        ++ "  EF "
        ++ Format.formatDays (scheduleEf schedule taskId)
        ++ "  LF "
        ++ Format.formatDays (scheduleLf schedule taskId)
        ++ "  LS "
        ++ Format.formatDays (scheduleLs schedule taskId)
        ++ "  Slack "
        ++ Format.formatDays (scheduleSlack schedule taskId)


estimateText : Estimate -> String
estimateText estimate =
    case estimate of
        Point days ->
            Format.formatDays days

        Range low high ->
            Format.formatDays low ++ "-" ++ Format.formatDays high


{-| Earliest/latest start times for every item, computed once per (tactic, items)
via a single forward and backward pass over a topologically sorted dependency
graph, rather than by re-walking the dependency tree from scratch for every
lookup.
-}
type alias Schedule =
    { es : Dict String Float
    , ef : Dict String Float
    , ls : Dict String Float
    , lf : Dict String Float
    }


buildSchedule : Tactic -> List Item -> Schedule
buildSchedule tactic items =
    let
        itemsById : Dict String Item
        itemsById =
            items
                |> List.map (\item -> ( taskIdToString (itemId item), item ))
                |> Dict.fromList

        dependentsOf : Dict String (List String)
        dependentsOf =
            items
                |> List.concatMap
                    (\item ->
                        itemDependsOn item
                            |> List.map (\dep -> ( taskIdToString dep, taskIdToString (itemId item) ))
                    )
                |> List.foldl
                    (\( depId, dependentId ) acc ->
                        Dict.update depId (\existing -> Just (dependentId :: Maybe.withDefault [] existing)) acc
                    )
                    Dict.empty

        topoOrder : List String
        topoOrder =
            topoSort itemsById dependentsOf

        durationOf : String -> Float
        durationOf id =
            case Dict.get id itemsById of
                Just (TaskItem task) ->
                    estimateDuration tactic task.estimate

                Just (MilestoneItem _) ->
                    0

                Nothing ->
                    0

        esDict : Dict String Float
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
                                                taskIdToString depId
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

        efDict : Dict String Float
        efDict =
            esDict |> Dict.map (\id esValue -> esValue + durationOf id)

        finish : Float
        finish =
            itemsById
                |> Dict.keys
                |> List.map (\id -> (Dict.get id esDict |> Maybe.withDefault 0) + durationOf id)
                |> List.maximum
                |> Maybe.withDefault 0

        lsDict : Dict String Float
        lsDict =
            List.foldl
                (\id acc ->
                    let
                        dependents =
                            Dict.get id dependentsOf |> Maybe.withDefault []

                        latestFinish =
                            case dependents of
                                [] ->
                                    finish

                                _ ->
                                    dependents
                                        |> List.map (\d -> Dict.get d acc |> Maybe.withDefault finish)
                                        |> List.minimum
                                        |> Maybe.withDefault finish
                    in
                    Dict.insert id (latestFinish - durationOf id) acc
                )
                Dict.empty
                (List.reverse topoOrder)

        lfDict : Dict String Float
        lfDict =
            lsDict |> Dict.map (\id lsValue -> lsValue + durationOf id)
    in
    { es = esDict, ef = efDict, ls = lsDict, lf = lfDict }


{-| Kahn's algorithm: repeatedly peel off items with no unprocessed
dependencies so each item is visited exactly once.
-}
topoSort : Dict String Item -> Dict String (List String) -> List String
topoSort itemsById dependentsOf =
    let
        inDegree0 : Dict String Int
        inDegree0 =
            itemsById |> Dict.map (\_ item -> List.length (itemDependsOn item))

        initialQueue : List String
        initialQueue =
            inDegree0 |> Dict.filter (\_ deg -> deg == 0) |> Dict.keys
    in
    topoSortHelp dependentsOf inDegree0 initialQueue []


topoSortHelp : Dict String (List String) -> Dict String Int -> List String -> List String -> List String
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
    Dict.get (taskIdToString taskId) schedule.es |> Maybe.withDefault 0


scheduleEf : Schedule -> TaskId -> Float
scheduleEf schedule taskId =
    Dict.get (taskIdToString taskId) schedule.ef |> Maybe.withDefault 0


scheduleLs : Schedule -> TaskId -> Float
scheduleLs schedule taskId =
    Dict.get (taskIdToString taskId) schedule.ls |> Maybe.withDefault 0


scheduleLf : Schedule -> TaskId -> Float
scheduleLf schedule taskId =
    Dict.get (taskIdToString taskId) schedule.lf |> Maybe.withDefault 0


scheduleSlack : Schedule -> TaskId -> Float
scheduleSlack schedule taskId =
    scheduleLs schedule taskId - scheduleEs schedule taskId


es : Tactic -> List Item -> TaskId -> Float
es tactic items taskId =
    scheduleEs (buildSchedule tactic items) taskId


ef : Tactic -> List Item -> TaskId -> Float
ef tactic items taskId =
    scheduleEf (buildSchedule tactic items) taskId


ls : Tactic -> List Item -> TaskId -> Float
ls tactic items taskId =
    scheduleLs (buildSchedule tactic items) taskId


lf : Tactic -> List Item -> TaskId -> Float
lf tactic items taskId =
    scheduleLf (buildSchedule tactic items) taskId


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


itemToElements : Schedule -> Bool -> Item -> List Encode.Value
itemToElements schedule showSpreadsheet item =
    let
        fields =
            itemFields schedule showSpreadsheet item

        nodeElement =
            Encode.object
                [ ( "data"
                  , Encode.object
                        ([ ( "id", encodeTaskId fields.id )
                         , ( "label", Encode.string fields.label )
                         , ( "kind", Encode.string fields.kind )
                         , ( "slack", Encode.float (scheduleSlack schedule fields.id) )
                         ]
                            ++ fields.card
                        )
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
    id


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
