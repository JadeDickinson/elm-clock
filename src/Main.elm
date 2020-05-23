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
  case Time.toMinute timeZone timeNow of
  0 ->
    ""
  1 ->
    ""
  2 ->
    ""
  3 ->
    ""
  4 ->
    ""
  5 ->
    "FIVE PAST "
  6 ->
    "FIVE PAST "
  7 ->
    "FIVE PAST "
  8 ->
    "FIVE PAST "
  9 ->
    "FIVE PAST "
  10 ->
    "TEN PAST "
  11 ->
    "TEN PAST "
  12 ->
    "TEN PAST "
  13 ->
    "TEN PAST "
  14 ->
    "TEN PAST "
  15 ->
    "QUARTER PAST "
  16 ->
    "QUARTER PAST "
  17 ->
    "QUARTER PAST "
  18 ->
    "QUARTER PAST "
  19 ->
    "QUARTER PAST "
  20 ->
    "TWENTY PAST "
  21 ->
    "TWENTY PAST "
  22 ->
    "TWENTY PAST "
  23 ->
    "TWENTY PAST "
  24 ->
    "TWENTY PAST "
  25 ->
    "TWENTY FIVE PAST "
  26 ->
    "TWENTY FIVE PAST "
  27 ->
    "TWENTY FIVE PAST "
  28 ->
    "TWENTY FIVE PAST "
  29 ->
    "TWENTY FIVE PAST "
  30 ->
    "HALF PAST "
  31 ->
    "HALF PAST "
  32 ->
    "HALF PAST "
  33 ->
    "HALF PAST "
  34 ->
    "HALF PAST "
  35 ->
    "TWENTY FIVE TO "
  36 ->
    "TWENTY FIVE TO "
  37 ->
    "TWENTY FIVE TO "
  38 ->
    "TWENTY FIVE TO "
  39 ->
    "TWENTY FIVE TO "
  40 ->
    "TWENTY TO "
  41 ->
    "TWENTY TO "
  42 ->
    "TWENTY TO "
  43 ->
    "TWENTY TO "
  44 ->
    "TWENTY TO "
  45 ->
    "QUARTER TO "
  46 ->
    "QUARTER TO "
  47 ->
    "QUARTER TO "
  48 ->
    "QUARTER TO "
  49 ->
    "QUARTER TO "
  50 ->
    "TEN TO "
  51 ->
    "TEN TO "
  52 ->
    "TEN TO "
  53 ->
    "TEN TO "
  54 ->
    "TEN TO "
  55 ->
    "FIVE TO "
  56 ->
    "FIVE TO "
  57 ->
    "FIVE TO "
  58 ->
    "FIVE TO "
  59 ->
    "FIVE TO "
  _ ->
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
  case Time.toMinute timeZone timeNow of
  0 ->
    " O CLOCK"
  1 ->
    " O CLOCK"
  2 ->
    " O CLOCK"
  3 ->
    " O CLOCK"
  4 ->
    " O CLOCK"
  _ ->
    ""
-- SUBSCRIPTIONS

wordIsHighlighted : String -> Model -> String
wordIsHighlighted word model =
  if String.contains word (getHour model.zone model.time) then
    "highlighted"
  else if String.contains word (getModifier model.zone model.time) then
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
        , span [class (wordIsHighlighted "QUARTER" model)] [text "QUARTER"]
        , span [] [text "DC"]
      ]
      , div [] [
        span [class (wordIsHighlighted "TWENTY" model)] [text "TWENTY"]
        , span [class (wordIsHighlighted "FIVE " model)] [text "FIVE"]
        , span [] [text "A"]
      ]
      , div [] [
        span [class (wordIsHighlighted "HALF" model)] [text "HALF"]
        , span [] [text "S"]
        , span [class (wordIsHighlighted "TEN " model)] [text "TEN"]
        , span [] [text "F"]
        , span [class (wordIsHighlighted "TO" model)] [text "TO"]
      ]
      , div [] [
        span [class (wordIsHighlighted "PAST" model)] [text "PAST"]
        , span [] [text "ERU"]
        , span [class (wordIsHighlighted "NINE" model)] [text "NINE"]
      ]
      , div [] [
        span [class (wordIsHighlighted "ONE" model)] [text "ONE"]
        , span [class (wordIsHighlighted "SIX" model)] [text "SIX"]
        , span [class (wordIsHighlighted "THREE" model)] [text "THREE"]
      ]
      , div [] [
        span [class (wordIsHighlighted "FOUR" model)] [text "FOUR"]
        , span [class (checkHourHighlighted "FIVE" model)] [text "FIVE"]
        , span [class (wordIsHighlighted "TWO" model)] [text "TWO"]
      ]
      , div [] [
        span [class (wordIsHighlighted "EIGHT" model)] [text "EIGHT"]
        , span [class (wordIsHighlighted "ELEVEN" model)] [text "ELEVEN"]
      ]
      , div [] [
        span [class (wordIsHighlighted "SEVEN" model)] [text "SEVEN"]
        , span [class (wordIsHighlighted "TWELVE" model)] [text "TWELVE"]
      ]
      , div [] [
        span [class (checkHourHighlighted "TEN" model)] [text "TEN"]
        , span [] [text "SE"]
        , span [class (checkOClockHighlighted model)] [text "OCLOCK"]
      ]
    ]
    ]
