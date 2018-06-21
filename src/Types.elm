module Types exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Time exposing (Time)

type alias Score =
    { right : Int
    , wrong : Int
    }

type Stage =
      Blank
    | Letters
    | Waiting
    | Guess

type Mode =
      IntroMode Int
    | BoxesMode Stage
    | ResultMode
    | SettingsMode
    | StatsMode

type alias Settings =
    ((Int, Int), Time)

type alias Model =
    { data : Array Char
    , scores : Dict Settings Score
    , dimensions : ( Int, Int )
    , mode : Mode
    , index : Int
    , recentCorrect : Maybe Bool
    , sideBarExpanded : Bool
    , waitingTime : Time
    }

type Msg =
      NoOp
    | SetMode Mode
    | NewIndex Int
    | KeyPress Char
    | SetData (Array Char)
    | SetSidebarExpansion Bool
    | SetWaitingTime Float
    | SetGridWidth Int

settings : Model -> Settings
settings model =
    (model.dimensions, model.waitingTime)

score : Model -> Score
score model =
    Dict.get (model.dimensions, model.waitingTime) model.scores
    |> Maybe.withDefault { right = 0, wrong = 0 }

size : Model -> Int
size model =
    case model.dimensions of
        ( w, h ) ->
            w * h

