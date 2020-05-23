module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
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


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0) (getModifier Time.utc (Time.millisToPosix 0)) (getHour Time.utc (Time.millisToPosix 0)) (getOClock Time.utc (Time.millisToPosix 0)) 
  , Task.perform AdjustTimeZone Time.here
  )



-- UPDATE


type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone



update : Msg -> Model -> (Model, Cmd Msg)
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
  case String.fromInt (Time.toHour timeZone timeNow) of
  "1" ->
    "ONE"
  "2" ->
    "TWO"
  "3" ->
    "THREE"
  "4" ->
    "FOUR"
  "5" ->
    "FIVE"
  "6" ->
    "SIX"
  "7" ->
    "SEVEN"
  "8" ->
    "EIGHT"
  "9" ->
    "NINE"
  "10" ->
    "TEN"
  "11" ->
    "ELEVEN"
  "12" ->
    "TWELVE"
  "13" ->
    "ONE"
  "14" ->
    "TWO"
  "15" ->
    "THREE"
  "16" ->
    "FOUR"
  "17" ->
    "FIVE"
  "18" ->
    "SIX"
  "19" ->
    "SEVEN"
  "20" ->
    "EIGHT"
  "21" ->
    "NINE"
  "22" ->
    "TEN"
  "23" ->
    "ELEVEN"
  _ ->
    ""

getOClock : Time.Zone -> Time.Posix -> String
getOClock timeZone timeNow = 
  let
    minutes =
      Time.toMinute timeZone timeNow
  in
    if minutes >= 0 && minutes <=4 then
      "OCLOCK"
    else
      ""
-- SUBSCRIPTIONS

checkModifierHighlighted : String -> Model -> String
checkModifierHighlighted word model =
  if String.contains word (getModifier model.zone model.time) then
    "highlighted"
  else
    ""

checkOClockHighlighted : Model -> String
checkOClockHighlighted model = 
  if String.contains "O CLOCK" (getOClock model.zone model.time) then
    "highlighted"
  else
    ""

checkHourHighlighted : String -> Model -> String
checkHourHighlighted word model = 
  if String.contains word (getHour model.zone model.time) then
    "highlighted"
  else
    ""

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick

-- VIEW


view : Model -> Html Msg
view model =
  let
    hour   = String.fromInt (Time.toHour   model.zone model.time)
    minute = String.fromInt (Time.toMinute model.zone model.time)
    second = String.fromInt (Time.toSecond model.zone model.time)
  in
  div [] [
    h1 [class "highlighted"] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
    , div [] [
      div [] [
        span [class "highlighted"] [text "IT"]
        , span [] [text "L"]
        , span [class "highlighted"] [text "IS"]
        , span [] [text "A"]
        , span [] [text "SAMPM"]
      ]
      , div [] [
        span [] [text "A"]
        , span [] [text "C"]
        , span [class (checkModifierHighlighted "QUARTER" model)] [text "QUARTER"]
        , span [] [text "DC"]
      ]
      , div [] [
        span [class (checkModifierHighlighted "TWENTY" model)] [text "TWENTY"]
        , span [class (checkModifierHighlighted "FIVE " model)] [text "FIVE"]
        , span [] [text "A"]
      ]
      , div [] [
        span [class (checkModifierHighlighted "HALF" model)] [text "HALF"]
        , span [] [text "S"]
        , span [class (checkModifierHighlighted "TEN " model)] [text "TEN"]
        , span [] [text "F"]
        , span [class (checkModifierHighlighted "TO" model)] [text "TO"]
      ]
      , div [] [
        span [class (checkModifierHighlighted "PAST" model)] [text "PAST"]
        , span [] [text "ERU"]
        , span [class (checkHourHighlighted "NINE" model)] [text "NINE"]
      ]
      , div [] [
        span [class (checkHourHighlighted "ONE" model)] [text "ONE"]
        , span [class (checkHourHighlighted "SIX" model)] [text "SIX"]
        , span [class (checkHourHighlighted "THREE" model)] [text "THREE"]
      ]
      , div [] [
        span [class (checkHourHighlighted "FOUR" model)] [text "FOUR"]
        , span [class (checkHourHighlighted "FIVE" model)] [text "FIVE"]
        , span [class (checkHourHighlighted "TWO" model)] [text "TWO"]
      ]
      , div [] [
        span [class (checkHourHighlighted "EIGHT" model)] [text "EIGHT"]
        , span [class (checkHourHighlighted "ELEVEN" model)] [text "ELEVEN"]
      ]
      , div [] [
        span [class (checkHourHighlighted "SEVEN" model)] [text "SEVEN"]
        , span [class (checkHourHighlighted "TWELVE" model)] [text "TWELVE"]
      ]
      , div [] [
        span [class (checkHourHighlighted "TEN" model)] [text "TEN"]
        , span [] [text "SE"]
        , span [class (checkOClockHighlighted model)] [text "OCLOCK"]
      ]
    ]
    ]
