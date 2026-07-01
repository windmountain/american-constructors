module Main exposing (Estimate(..), Item(..), Milestone, Task, TaskId(..), es, main)

import Browser
import Csv.Decode as Decode exposing (Decoder)
import Data
import Format
import Html exposing (Html, node, pre, text)
import Html.Attributes exposing (property)
import Json.Encode as Encode


type TaskId
    = TaskId Int


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
    Decode.map3 (\a b c -> List.filterMap identity [ a, b, c ])
        (optionalIntField "Deps on (1)")
        (optionalIntField "Deps on (2)")
        (optionalIntField "Deps on (3)")
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
    {}


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Browser.Document Msg
view _ =
    { title = "AC Tasks"
    , body =
        [ case itemsResult of
            Ok items ->
                viewGraph items

            Err error ->
                pre [] [ text (Decode.errorToString error) ]
        ]
    }


viewGraph : List Item -> Html Msg
viewGraph items =
    node "cytoscape-graph" [ property "elements" (encodeElements items) ] []


encodeElements : List Item -> Encode.Value
encodeElements items =
    Encode.list identity (List.concatMap (itemToElements items) items)


itemFields : List Item -> Item -> { id : TaskId, label : String, dependsOn : List TaskId, kind : String }
itemFields items item =
    case item of
        TaskItem task ->
            { id = task.id
            , label = String.join "\n" [ "[" ++ task.section ++ "]", task.name ++ " (" ++ estimateText task.estimate ++ ")", esText items task.id ]
            , dependsOn = task.dependsOn
            , kind = "task"
            }

        MilestoneItem milestone ->
            { id = milestone.id
            , label = String.join "\n" [ "[" ++ milestone.section ++ "]", milestone.name, esText items milestone.id ]
            , dependsOn = milestone.dependsOn
            , kind = "milestone"
            }


esText : List Item -> TaskId -> String
esText items taskId =
    "ES " ++ Format.formatDays (es items taskId) ++ "d"


estimateText : Estimate -> String
estimateText estimate =
    case estimate of
        Point days ->
            Format.formatDays days ++ "d"

        Range low high ->
            Format.formatDays low ++ "-" ++ Format.formatDays high ++ "d"


es : List Item -> TaskId -> Float
es items taskId =
    findItem items taskId
        |> Maybe.map
            (\item ->
                itemDependsOn item
                    |> List.map (\depId -> es items depId + duration items depId)
                    |> List.maximum
                    |> Maybe.withDefault 0
            )
        |> Maybe.withDefault 0


duration : List Item -> TaskId -> Float
duration items taskId =
    case findItem items taskId of
        Just (TaskItem task) ->
            case task.estimate of
                Point days ->
                    days

                Range _ high ->
                    high

        Just (MilestoneItem _) ->
            0

        Nothing ->
            0


findItem : List Item -> TaskId -> Maybe Item
findItem items taskId =
    items
        |> List.filter (\item -> itemId item == taskId)
        |> List.head


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


itemToElements : List Item -> Item -> List Encode.Value
itemToElements items item =
    let
        fields =
            itemFields items item

        nodeElement =
            Encode.object
                [ ( "data"
                  , Encode.object
                        [ ( "id", encodeTaskId fields.id )
                        , ( "label", Encode.string fields.label )
                        , ( "kind", Encode.string fields.kind )
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
