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
import Set exposing (Set)


type TaskId
    = TaskId String


type Tactic
    = Optimistic
    | Pessimistic
    | Midpoint


type ViewMode
    = NetworkView
    | CalendarView


type WorkdayMode
    = Conservative
    | Aggressive


type Estimate
    = Point Float
    | Range Float Float


type alias Task =
    { id : TaskId
    , section : String
    , name : String
    , dependsOn : List TaskId
    , estimate : Estimate
    , isEffectiveEnd : Bool
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
    , isEffectiveEnd : Bool
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
    , isEffectiveEnd : Bool
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
        |> Decode.pipeline (yesNoDecoder "Effective End")
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
                , isEffectiveEnd = fields.isEffectiveEnd
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
                , isEffectiveEnd = fields.isEffectiveEnd
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
    , workdayMode : WorkdayMode
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tactic = Midpoint, showSpreadsheet = False, viewMode = NetworkView, workdayMode = Conservative }, Cmd.none )


type Msg
    = SetTactic Tactic
    | ToggleShowSpreadsheet
    | SetViewMode ViewMode
    | SetWorkdayMode WorkdayMode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTactic tactic ->
            ( { model | tactic = tactic }, Cmd.none )

        ToggleShowSpreadsheet ->
            ( { model | showSpreadsheet = not model.showSpreadsheet }, Cmd.none )

        SetViewMode viewMode ->
            ( { model | viewMode = viewMode }, Cmd.none )

        SetWorkdayMode workdayMode ->
            ( { model | workdayMode = workdayMode }, Cmd.none )


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
                viewMain model.viewMode model.tactic showSpreadsheet model.workdayMode items

            Err error ->
                pre [] [ text (Decode.errorToString error) ]
        , viewDownloads
        , if model.tactic == Midpoint then
            viewSpreadsheetToggle model.showSpreadsheet

          else
            text ""
        , if model.viewMode == CalendarView then
            viewWorkdayModeSelect model.workdayMode

          else
            text ""
        ]
    }


viewMain : ViewMode -> Tactic -> Bool -> WorkdayMode -> List Item -> Html Msg
viewMain viewMode tactic showSpreadsheet workdayMode items =
    case viewMode of
        NetworkView ->
            viewGraph tactic showSpreadsheet items

        CalendarView ->
            viewCalendar workdayMode tactic items


{-| The project only spans the last four months of 2009, and always will,
so the months and their date-to-weekday mapping are hardcoded rather than
computed.
-}
type alias MonthSpec =
    { name : String
    , firstWeekday : Int
    , daysInMonth : Int
    }


calendarMonths : List MonthSpec
calendarMonths =
    [ { name = "September", firstWeekday = 2, daysInMonth = 30 }
    , { name = "October", firstWeekday = 4, daysInMonth = 31 }
    , { name = "November", firstWeekday = 0, daysInMonth = 30 }
    , { name = "December", firstWeekday = 2, daysInMonth = 31 }
    ]


weekdayLabels : List String
weekdayLabels =
    [ "S", "M", "T", "W", "T", "F", "S" ]


{-| A day's position in the overall Sept-Dec span, for comparing dates
that may fall in different months (e.g. is this day between the project
start and the desired finish).
-}
dayIndex : String -> Int -> Int
dayIndex monthName day =
    let
        daysBeforeMonth : List ( String, Int )
        daysBeforeMonth =
            calendarMonths
                |> List.foldl (\month ( entries, total ) -> ( entries ++ [ ( month.name, total ) ], total + month.daysInMonth )) ( [], 0 )
                |> Tuple.first
    in
    (daysBeforeMonth |> List.filter (\( name, _ ) -> name == monthName) |> List.head |> Maybe.map Tuple.second |> Maybe.withDefault 0) + day


{-| The project's actual first day (2009-09-24), for marking on the
calendar view.
-}
projectStart : { month : String, day : Int }
projectStart =
    { month = "September", day = 24 }


{-| The desired finish date (2009-12-14), fixed regardless of what the
schedule estimate says - this is what the estimate will ultimately be
compared against.
-}
desiredFinish : { month : String, day : Int }
desiredFinish =
    { month = "December", day = 14 }


type alias Holiday =
    { month : String
    , day : Int
    , name : String
    }


holidays : List Holiday
holidays =
    [ { month = "November", day = 11, name = "Veterans Day" }
    , { month = "November", day = 26, name = "Thanksgiving" }
    ]


{-| Whether a day is the right *kind* of day to be worked - not a holiday,
and (unless aggressive mode allows weekends) not a weekend - independent of
whether it falls within the project's highlighted date range. Takes
isWeekend rather than computing it, since callers can derive it either from
a grid index (dayCell) or from the date itself (workingDaysCount,
actualWorkDayKeys).
-}
isWorkdayEligible : WorkdayMode -> String -> Int -> Bool -> Bool
isWorkdayEligible workdayMode monthName day isWeekend =
    let
        isHoliday =
            List.any (\holiday -> holiday.month == monthName && holiday.day == day) holidays

        countsAsWorkday =
            case workdayMode of
                Conservative ->
                    not isWeekend

                Aggressive ->
                    True
    in
    countsAsWorkday && not isHoliday


{-| A day counts as a working day (for the calendar's light-blue range
highlight) if it's workday-eligible and falls within the project's date
range (projectStart..desiredFinish). Actual work days (actualWorkDayKeys)
are not bound by this range, since the critical duration can run past it.
-}
isWorkingDayGiven : WorkdayMode -> String -> Int -> Bool -> Bool
isWorkingDayGiven workdayMode monthName day isWeekend =
    let
        idx =
            dayIndex monthName day

        inRange =
            idx >= dayIndex projectStart.month projectStart.day && idx <= dayIndex desiredFinish.month desiredFinish.day
    in
    inRange && isWorkdayEligible workdayMode monthName day isWeekend


workingDaysCount : WorkdayMode -> Int
workingDaysCount workdayMode =
    calendarMonths
        |> List.concatMap
            (\month ->
                List.range 1 month.daysInMonth
                    |> List.map
                        (\day ->
                            let
                                weekday =
                                    modBy 7 (month.firstWeekday + day - 1)
                            in
                            isWorkingDayGiven workdayMode month.name day (weekday == 0 || weekday == 6)
                        )
            )
        |> List.filter identity
        |> List.length


dayKey : String -> Int -> String
dayKey monthName day =
    monthName ++ "-" ++ String.fromInt day


{-| The critical path's duration in working days, measured to the
"occupancy permitted" milestone - the item marked as the effective end
(see buildSchedule). That item's ES/EF/LS/LF are always equal to each
other (it's the anchor the whole critical path is measured against), so
any of the four would do; EF is used here as "days elapsed to reach it".
-}
criticalDuration : Tactic -> List Item -> Float
criticalDuration tactic items =
    let
        schedule =
            buildSchedule tactic items
    in
    items
        |> List.filter itemIsEffectiveEnd
        |> List.head
        |> Maybe.map (itemId >> scheduleEf schedule)
        |> Maybe.withDefault 0


{-| The actual work days: starting from the project start, walk the
calendar day by day counting only working days (per the selected workday
mode), marking each one as an actual work day until `neededCount` of them
have been marked. Can run past the desired finish date into the calendar's
remaining days; if the critical duration is long enough to run past the
last day on the calendar (2009-12-31), `overflowed` is set so the caller
can show a continuation marker instead of the missing days themselves,
since this calendar never shows any other period.
-}
actualWorkDayKeys : WorkdayMode -> Int -> { keys : Set String, overflowed : Bool }
actualWorkDayKeys workdayMode neededCount =
    let
        onOrAfterStart : MonthSpec -> Int -> Bool
        onOrAfterStart month day =
            dayIndex month.name day >= dayIndex projectStart.month projectStart.day

        step : MonthSpec -> Int -> ( Set String, Int ) -> ( Set String, Int )
        step month day ( acc, remaining ) =
            if remaining <= 0 || not (onOrAfterStart month day) then
                ( acc, remaining )

            else
                let
                    weekday =
                        modBy 7 (month.firstWeekday + day - 1)
                in
                if isWorkdayEligible workdayMode month.name day (weekday == 0 || weekday == 6) then
                    ( Set.insert (dayKey month.name day) acc, remaining - 1 )

                else
                    ( acc, remaining )

        ( keys, leftover ) =
            calendarMonths
                |> List.foldl
                    (\month acc -> List.foldl (step month) acc (List.range 1 month.daysInMonth))
                    ( Set.empty, neededCount )
    in
    { keys = keys, overflowed = leftover > 0 }


viewCalendar : WorkdayMode -> Tactic -> List Item -> Html msg
viewCalendar workdayMode tactic items =
    let
        neededCount : Int
        neededCount =
            ceiling (criticalDuration tactic items)

        actualWork : { keys : Set String, overflowed : Bool }
        actualWork =
            actualWorkDayKeys workdayMode neededCount

        {- Positive: the actual work finishes with this many working days of
           the Sept24-Dec14 window unused. Negative: it overruns the window
           by this many working days.
        -}
        daysToSpare : Int
        daysToSpare =
            workingDaysCount workdayMode - neededCount

        {- Whether the schedule fits even in the worst case: pessimistic
           estimates under the strictest (conservative) working day
           assumptions. Computed independent of the currently selected
           tactic/workday mode.
        -}
        fitsWorstCase : Bool
        fitsWorstCase =
            ceiling (criticalDuration Pessimistic items) <= workingDaysCount Conservative
    in
    div
        [ style "font-family" "-apple-system, BlinkMacSystemFont, sans-serif" ]
        [ viewCalendarHeader workdayMode
        , div
            [ style "display" "grid"
            , style "grid-template-columns" "1fr 1fr"
            , style "gap" "40px"
            , style "padding" "24px 48px"
            ]
            (List.map (viewMonth workdayMode actualWork.keys actualWork.overflowed) calendarMonths)
        , viewOnTimeSummary daysToSpare fitsWorstCase
        ]


viewOnTimeSummary : Int -> Bool -> Html msg
viewOnTimeSummary daysToSpare fitsWorstCase =
    div
        [ style "text-align" "center"
        , style "font-size" "18px"
        , style "padding" "24px 0 32px 0"
        ]
        [ text
            ((if daysToSpare < 0 then
                "The project will not be done on time. It will be " ++ String.fromInt (negate daysToSpare) ++ " days late."

              else
                "The project will be done on time with " ++ String.fromInt daysToSpare ++ " days to spare."
             )
                ++ (if fitsWorstCase then
                        " Hooray!"

                    else
                        ""
                   )
            )
        ]


viewCalendarHeader : WorkdayMode -> Html msg
viewCalendarHeader workdayMode =
    div
        [ style "text-align" "center"
        , style "padding-top" "24px"
        ]
        [ div [ style "font-size" "36px", style "font-weight" "bold" ] [ text "2009" ]
        , div
            [ style "font-size" "16px", style "color" "#4b5563" ]
            [ text (String.fromInt (workingDaysCount workdayMode) ++ " working days left") ]
        ]


viewMonth : WorkdayMode -> Set String -> Bool -> MonthSpec -> Html msg
viewMonth workdayMode actualWorkDays overflowed month =
    let
        {- The grid position right after the month's last real day - for
           December, that's the tile that would otherwise sit at 2010-01-01.
           Rather than show a date from the next year, that single tile is
           repurposed as a continuation marker when the actual work days run
           past the visible calendar.
        -}
        overflowIndex : Int
        overflowIndex =
            month.firstWeekday + month.daysInMonth

        renderCell : Int -> Maybe Int -> Html msg
        renderCell index day =
            if overflowed && month.name == "December" && index == overflowIndex then
                overflowCell

            else
                dayCell month.name workdayMode actualWorkDays index day
    in
    div []
        [ div
            [ style "text-align" "center"
            , style "font-size" "24px"
            , style "margin-bottom" "8px"
            ]
            [ text month.name ]
        , div
            [ style "display" "grid"
            , style "grid-template-columns" "repeat(7, 1fr)"
            ]
            (List.map weekdayHeaderCell weekdayLabels ++ List.indexedMap renderCell (monthCells month))
        ]


overflowCell : Html msg
overflowCell =
    div
        [ style "text-align" "center"
        , style "padding" "6px"
        , style "border" "1px solid #d1d5db"
        , style "color" "#dc2626"
        , style "font-size" "11px"
        ]
        [ text "(and beyond!)" ]


weekdayHeaderCell : String -> Html msg
weekdayHeaderCell label_ =
    div
        [ style "text-align" "center"
        , style "padding" "6px"
        , style "font-weight" "bold"
        ]
        [ text label_ ]


{-| One cell per grid position, padded with Nothing before day 1 and after
the last day so every month lines up under the S M T W T F S header.
-}
monthCells : MonthSpec -> List (Maybe Int)
monthCells month =
    let
        cells : List (Maybe Int)
        cells =
            List.repeat month.firstWeekday Nothing ++ List.map Just (List.range 1 month.daysInMonth)

        remainder : Int
        remainder =
            modBy 7 (List.length cells)
    in
    cells
        ++ (if remainder == 0 then
                []

            else
                List.repeat (7 - remainder) Nothing
           )


dayCell : String -> WorkdayMode -> Set String -> Int -> Maybe Int -> Html msg
dayCell monthName workdayMode actualWorkDays index day =
    let
        isWeekend : Bool
        isWeekend =
            modBy 7 index == 0 || modBy 7 index == 6

        isHoliday : Bool
        isHoliday =
            List.any (\holiday -> holiday.month == monthName && Just holiday.day == day) holidays

        isProjectStart : Bool
        isProjectStart =
            monthName == projectStart.month && day == Just projectStart.day

        isDesiredFinish : Bool
        isDesiredFinish =
            monthName == desiredFinish.month && day == Just desiredFinish.day

        isWorkingDay : Bool
        isWorkingDay =
            case day of
                Nothing ->
                    False

                Just d ->
                    isWorkingDayGiven workdayMode monthName d isWeekend

        isActualWorkDay : Bool
        isActualWorkDay =
            case day of
                Nothing ->
                    False

                Just d ->
                    Set.member (dayKey monthName d) actualWorkDays

        isAfterDesiredFinish : Bool
        isAfterDesiredFinish =
            case day of
                Nothing ->
                    False

                Just d ->
                    dayIndex monthName d > dayIndex desiredFinish.month desiredFinish.day
    in
    div
        [ style "text-align" "center"
        , style "padding" "6px"
        , style "border"
            (if isProjectStart then
                "3px solid #2563eb"

             else if isDesiredFinish then
                "3px solid #dc2626"

             else
                "1px solid #d1d5db"
            )
        , style "font-weight"
            (if isProjectStart || isDesiredFinish then
                "bold"

             else
                "normal"
            )
        , style "background"
            (if isActualWorkDay && isAfterDesiredFinish then
                "#fecaca"

             else if isActualWorkDay then
                "#60a5fa"

             else if isWorkingDay then
                "#dbeafe"

             else if isWeekend || isHoliday then
                "#e5e7eb"

             else
                "transparent"
            )
        ]
        [ text (Maybe.withDefault "" (Maybe.map String.fromInt day)) ]


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


{-| Only shown in calendar view, top right corner: which days count as
working days when comparing the schedule estimate against the calendar.
Conservative counts only non-holiday weekdays; aggressive also counts
weekends, excluding just the holidays.
-}
viewWorkdayModeSelect : WorkdayMode -> Html Msg
viewWorkdayModeSelect current =
    div
        [ style "position" "fixed"
        , style "top" "16px"
        , style "right" "16px"
        , style "z-index" "20"
        , style "font-family" "-apple-system, BlinkMacSystemFont, sans-serif"
        , style "font-size" "12px"
        ]
        [ label [ for "workday-mode-select" ] [ text "working day assumption: " ]
        , select [ id "workday-mode-select", onInput (workdayModeFromString >> Maybe.withDefault current >> SetWorkdayMode) ]
            [ option [ value "conservative", selected (current == Conservative) ] [ text "Conservative" ]
            , option [ value "aggressive", selected (current == Aggressive) ] [ text "Aggressive" ]
            ]
        ]


workdayModeFromString : String -> Maybe WorkdayMode
workdayModeFromString value_ =
    case value_ of
        "conservative" ->
            Just Conservative

        "aggressive" ->
            Just Aggressive

        _ ->
            Nothing


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
                , ( "lf", Encode.string (Format.formatMaybeDays (scheduleLf schedule task.id)) )
                , ( "ls", Encode.string (Format.formatMaybeDays (scheduleLs schedule task.id)) )
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
                , ( "lf", Encode.string (Format.formatMaybeDays (scheduleLf schedule milestone.id)) )
                , ( "ls", Encode.string (Format.formatMaybeDays (scheduleLs schedule milestone.id)) )
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
        ++ Format.formatMaybeDays (scheduleLf schedule taskId)
        ++ "  LS "
        ++ Format.formatMaybeDays (scheduleLs schedule taskId)
        ++ "  Slack "
        ++ Format.formatMaybeDays (scheduleSlack schedule taskId)


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

        {- The critical path is anchored to a single, explicitly-marked end
           item (see the "Effective End" column) rather than to every item
           with no dependents. ES/EF stay defined for every item - they're
           just how early something can happen, regardless of any end date -
           but LS/LF (and therefore slack) only make sense for items that
           actually lead up to that end item. Anything else, including tasks
           scheduled after it, gets no LS/LF at all rather than a value
           computed against the wrong finish date.
        -}
        endId : Maybe String
        endId =
            items
                |> List.filter itemIsEffectiveEnd
                |> List.head
                |> Maybe.map (itemId >> taskIdToString)

        criticalPathIds : Set String
        criticalPathIds =
            case endId of
                Just id ->
                    ancestorsOf itemsById id

                Nothing ->
                    itemsById |> Dict.keys |> Set.fromList

        criticalPathTopoOrder : List String
        criticalPathTopoOrder =
            topoOrder |> List.filter (\id -> Set.member id criticalPathIds)

        criticalPathDependentsOf : Dict String (List String)
        criticalPathDependentsOf =
            dependentsOf
                |> Dict.filter (\id _ -> Set.member id criticalPathIds)
                |> Dict.map (\_ deps -> List.filter (\d -> Set.member d criticalPathIds) deps)

        finish : Float
        finish =
            case endId of
                Just id ->
                    Dict.get id efDict |> Maybe.withDefault 0

                Nothing ->
                    topoOrder
                        |> List.map (\id -> Dict.get id efDict |> Maybe.withDefault 0)
                        |> List.maximum
                        |> Maybe.withDefault 0

        lsDict : Dict String Float
        lsDict =
            List.foldl
                (\id acc ->
                    let
                        dependents =
                            Dict.get id criticalPathDependentsOf |> Maybe.withDefault []

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
                (List.reverse criticalPathTopoOrder)

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


{-| Every item an item transitively depends on, plus the item itself -
its "ancestors" in schedule order. Used to scope critical path
calculations to just the items that lead up to the effective end item.
-}
ancestorsOf : Dict String Item -> String -> Set String
ancestorsOf itemsById startId =
    ancestorsOfHelp itemsById [ startId ] Set.empty


ancestorsOfHelp : Dict String Item -> List String -> Set String -> Set String
ancestorsOfHelp itemsById queue visited =
    case queue of
        [] ->
            visited

        id :: rest ->
            if Set.member id visited then
                ancestorsOfHelp itemsById rest visited

            else
                let
                    deps =
                        Dict.get id itemsById
                            |> Maybe.map (itemDependsOn >> List.map taskIdToString)
                            |> Maybe.withDefault []
                in
                ancestorsOfHelp itemsById (deps ++ rest) (Set.insert id visited)


scheduleEs : Schedule -> TaskId -> Float
scheduleEs schedule taskId =
    Dict.get (taskIdToString taskId) schedule.es |> Maybe.withDefault 0


scheduleEf : Schedule -> TaskId -> Float
scheduleEf schedule taskId =
    Dict.get (taskIdToString taskId) schedule.ef |> Maybe.withDefault 0


{-| Nothing means this item isn't an ancestor of the effective end item, so
it has no meaningful late-start/late-finish (and therefore no slack).
-}
scheduleLs : Schedule -> TaskId -> Maybe Float
scheduleLs schedule taskId =
    Dict.get (taskIdToString taskId) schedule.ls


scheduleLf : Schedule -> TaskId -> Maybe Float
scheduleLf schedule taskId =
    Dict.get (taskIdToString taskId) schedule.lf


scheduleSlack : Schedule -> TaskId -> Maybe Float
scheduleSlack schedule taskId =
    scheduleLs schedule taskId |> Maybe.map (\lsValue -> lsValue - scheduleEs schedule taskId)


es : Tactic -> List Item -> TaskId -> Float
es tactic items taskId =
    scheduleEs (buildSchedule tactic items) taskId


ef : Tactic -> List Item -> TaskId -> Float
ef tactic items taskId =
    scheduleEf (buildSchedule tactic items) taskId


ls : Tactic -> List Item -> TaskId -> Maybe Float
ls tactic items taskId =
    scheduleLs (buildSchedule tactic items) taskId


lf : Tactic -> List Item -> TaskId -> Maybe Float
lf tactic items taskId =
    scheduleLf (buildSchedule tactic items) taskId


slack : Tactic -> List Item -> TaskId -> Maybe Float
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


itemIsEffectiveEnd : Item -> Bool
itemIsEffectiveEnd item =
    case item of
        TaskItem task ->
            task.isEffectiveEnd

        MilestoneItem milestone ->
            milestone.isEffectiveEnd


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
                         , ( "slack", Encode.string (Format.formatMaybeDays (scheduleSlack schedule fields.id)) )
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
