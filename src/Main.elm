module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import NumberToWords
import Task
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    , modifier : String
    , hour : String
    , oClock : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0) (getModifier Time.utc (Time.millisToPosix 0)) (getHour Time.utc (Time.millisToPosix 0)) (getOClock Time.utc (Time.millisToPosix 0))
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime, modifier = getModifier model.zone newTime, hour = getHour model.zone newTime, oClock = getOClock model.zone newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )


getModifier : Time.Zone -> Time.Posix -> String
getModifier timeZone timeNow =
    let
        minutes =
            Time.toMinute timeZone timeNow
    in
    if minutes >= 0 && minutes <= 4 then
        ""

    else if minutes >= 5 && minutes <= 9 then
        "FIVE PAST"

    else if minutes >= 10 && minutes <= 14 then
        "TEN PAST"

    else if minutes >= 15 && minutes <= 19 then
        "QUARTER PAST"

    else if minutes >= 20 && minutes <= 24 then
        "TWENTY PAST"

    else if minutes >= 25 && minutes <= 29 then
        "TWENTY FIVE PAST"

    else if minutes >= 30 && minutes <= 34 then
        "HALF PAST"

    else if minutes >= 35 && minutes <= 39 then
        "TWENTY FIVE TO"

    else if minutes >= 40 && minutes <= 44 then
        "TWENTY TO"

    else if minutes >= 45 && minutes <= 49 then
        "QUARTER TO"

    else if minutes >= 50 && minutes <= 54 then
        "TEN TO"

    else if minutes >= 55 && minutes <= 59 then
        "FIVE TO"

    else
        ""


getHour : Time.Zone -> Time.Posix -> String
getHour timeZone timeNow =
    String.toUpper (NumberToWords.intToWords (modBy 12 (Time.toHour timeZone timeNow)))


getOClock : Time.Zone -> Time.Posix -> String
getOClock timeZone timeNow =
    let
        minutes =
            Time.toMinute timeZone timeNow
    in
    if minutes >= 0 && minutes <= 4 then
        ""

    else
        ""



-- SUBSCRIPTIONS


checkModifierHighlighted : String -> Model -> String
checkModifierHighlighted word model =
    if String.contains word (getModifier model.zone model.time) then
        "highlighted"

    else
        ""


checkHourHighlighted : String -> Model -> String
checkHourHighlighted word model =
    if String.contains word (getHour model.zone model.time) then
        "highlighted"

    else
        ""



-- "QUARTER"


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW


manualCss =
    Html.node "style"
        []
        [ Html.text <|
            """
            body,
            html {
            font-family: "DIN Alternate";
            color: #e2dbd8;
            background-color: red;
            letter-spacing: 11px;
            }

            .highlighted {
            color: black;
            }
            """
        ]


view : Model -> Html Msg
view model =
    let
        hour =
            String.fromInt (Time.toHour model.zone model.time)

        minute =
            String.fromInt (Time.toMinute model.zone model.time)

        second =
            String.fromInt (Time.toSecond model.zone model.time)

        lang =
            Spanish
    in
    div []
        [ manualCss
        , h1 [ class "highlighted" ] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
        , div []
            [ div []
                [ span [ class "highlighted" ] [ text (translate lang "IT") ]
                , span [] [ text "L" ]
                , span [ class "highlighted" ] [ text (translate lang "IS") ]
                , span [] [ text "A" ]
                , span [] [ text "SAMPM" ]
                ]
            , div []
                [ span [] [ text "A" ]
                , span [] [ text "C" ]
                , span [ class (checkModifierHighlighted (translate lang "QUARTER") model) ] [ text (translate lang "QUARTER") ]
                , span [] [ text "DC" ]
                ]
            , div []
                [ span [ class (checkModifierHighlighted (translate lang "TWENTY") model) ] [ text (translate lang "TWENTY") ]
                , span [ class (checkModifierHighlighted (translate lang "FIVE ") model) ] [ text (translate lang "FIVE") ]
                , span [] [ text "A" ]
                ]
            , div []
                [ span [ class (checkModifierHighlighted (translate lang "HALF") model) ] [ text (translate lang "HALF") ]
                , span [] [ text "S" ]
                , span [ class (checkModifierHighlighted (translate lang "TEN ") model) ] [ text (translate lang "TEN") ]
                , span [] [ text "F" ]
                , span [ class (checkModifierHighlighted (translate lang "TO") model) ] [ text (translate lang "TO") ]
                ]
            , div []
                [ span [ class (checkModifierHighlighted (translate lang "PAST") model) ] [ text (translate lang "PAST") ]
                , span [] [ text "ERU" ]
                , span [ class (checkHourHighlighted (translate lang "NINE") model) ] [ text (translate lang "NINE") ]
                ]
            , div []
                [ span [ class (checkHourHighlighted (translate lang "ONE") model) ] [ text (translate lang "ONE") ]
                , span [ class (checkHourHighlighted (translate lang "SIX") model) ] [ text (translate lang "SIX") ]
                , span [ class (checkHourHighlighted (translate lang "THREE") model) ] [ text (translate lang "THREE") ]
                ]
            , div []
                [ span [ class (checkHourHighlighted (translate lang "FOUR") model) ] [ text (translate lang "FOUR") ]
                , span [ class (checkHourHighlighted (translate lang "FIVE") model) ] [ text (translate lang "FIVE") ]
                , span [ class (checkHourHighlighted (translate lang "TWO") model) ] [ text (translate lang "TWO") ]
                ]
            , div []
                [ span [ class (checkHourHighlighted (translate lang "EIGHT") model) ] [ text (translate lang "EIGHT") ]
                , span [ class (checkHourHighlighted (translate lang "ELEVEN") model) ] [ text (translate lang "ELEVEN") ]
                ]
            , div []
                [ span [ class (checkHourHighlighted (translate lang "SEVEN") model) ] [ text (translate lang "SEVEN") ]
                , span [ class (checkHourHighlighted (translate lang "TWELVE") model) ] [ text (translate lang "TWELVE") ]
                ]
            , div []
                [ span [ class (checkHourHighlighted (translate lang "TEN") model) ] [ text (translate lang "TEN") ]
                , span [] [ text "SE" ]
                , span [ class (checkModifierHighlighted (translate lang "OCLOCK") model) ] [ text (translate lang "OCLOCK") ]
                ]
            ]
        ]


type Language
    = English
    | Spanish


translate : Language -> String -> String
translate language string =
    case language of
        Spanish ->
            toSpanish string

        English ->
            string


toSpanish : String -> String
toSpanish s =
    case s of
        "IT" ->
            "SON"

        "IS" ->
            "LAS"

        "QUARTER" ->
            "QUARTER"

        "HALF" ->
            "HALF"

        "TO" ->
            "TO"

        "PAST" ->
            "PAST"

        "OCLOCK" ->
            "ENPUNTO"

        "ONE" ->
            "UNO"

        "TWO" ->
            "DOS"

        "THREE" ->
            "TRES"

        "FOUR" ->
            "QUATRO"

        "FIVE" ->
            "CINCO"

        "SIX" ->
            "SEIS"

        "SEVEN" ->
            "SIETE"

        "EIGHT" ->
            "OCHO"

        "NINE" ->
            "NUEVE"

        "TEN" ->
            "DIEZ"

        "ELEVEN" -> "ONCE"

        "TWELVE" -> "DOCE"

        "TWENTY" ->
            "VEINTI"

        "HALF PAST" -> "MEDIA"

        _ ->
            "UNKNOWN:" ++ s
