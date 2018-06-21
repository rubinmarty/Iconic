import View exposing (..)
import Types exposing (..)

import Dict exposing (Dict)
import Array exposing (Array)
import Keyboard exposing (KeyCode)
import Char
import Html exposing (program)
import Time
import Random
import Random.Char as Random
import Random.Array as Random


main : Program Never Model Msg
main =
    Html.program
        { init = init ! [newChars <| size init]
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

init : Model
init =
    { data = Array.empty
    , scores = Dict.empty
    , dimensions = (4, 3)
    , mode = IntroMode 0
    , index = -1
    , recentCorrect = Nothing
    , sideBarExpanded = False
    , waitingTime = 0
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []
        SetMode mode ->
            let
                command =
                    case mode of
                        BoxesMode Guess ->
                            Random.generate NewIndex (Random.int 0 (size model - 1))
                        _ ->
                            Cmd.none

            in
                { model | mode = mode } ! [command]
        NewIndex i ->
            {model | index = i } ! []
        KeyPress ch ->
            keypress ch model
        SetData chs ->
            { model | data = chs, index = -1 } ! []
        SetSidebarExpansion b ->
            { model | sideBarExpanded = b } ! []
        SetWaitingTime fl ->
            { model | waitingTime = fl
                    , mode = ResultMode
                    , recentCorrect = Nothing
            } ! []
        SetGridWidth x ->
            { model | dimensions = (x, Tuple.second model.dimensions)
                    , mode = ResultMode
                    , recentCorrect = Nothing
            } ! []


newChars : Int -> Cmd Msg
newChars size =
    Random.generate SetData (makeData <| size)


makeData : Int -> Random.Generator (Array Char)
makeData x =
    Random.array x Random.upperCaseLatin

addRight : Score -> Score
addRight score =
    { score | right = score.right + 1 }

addWrong : Score -> Score
addWrong score =
    { score | wrong = score.wrong + 1 }

updateScores : Model -> Bool -> Model
updateScores model isRight =
    score model
    |> (if isRight then addRight else addWrong)
    |> (\v -> Dict.insert (settings model) v model.scores)
    |> (\s -> { model | scores = s })

keypress : Char -> Model -> ( Model, Cmd Msg )
keypress ch model =
    case model.mode of
        IntroMode i ->
            let
                newMode =
                    if i == 2 then BoxesMode Blank
                    else IntroMode <| i + 1
            in
                { model | mode = newMode } ! []
            
        BoxesMode Guess ->
            let
                guess =
                    Char.toUpper ch
                isCorrect =
                    Array.get model.index model.data == Maybe.Just guess
            in
                if Char.isUpper guess then
                    updateScores { model |
                        mode = ResultMode
                      , recentCorrect = Just isCorrect
                    } isCorrect ! []
                else
                    model ! []

        BoxesMode _ ->
            model ! []

        ResultMode ->
            { model | mode = BoxesMode Blank } ! [newChars <| size model]

        SettingsMode ->
            if ch == (Char.fromCode 13)
            then { model | mode = BoxesMode Blank } ! []
            else model ! []

        StatsMode ->
            if ch == (Char.fromCode 13)
            then { model | mode = BoxesMode Blank } ! []
            else model ! []



guess : Char -> Model -> Model
guess ch model =
    Array.get model.index model.data
    |> Maybe.map ((==) ch)
    |> Maybe.withDefault False
    |> updateScores model


-- SUBSCRIPTIONS

blankTime = Time.second * 4
lettersTime = Time.millisecond * 50

subscriptions : Model -> Sub Msg
subscriptions model =
    let
        modeSubscription =
            case model.mode of
                IntroMode i ->
                    Keyboard.presses (\kc -> KeyPress <| Char.fromCode kc)
                BoxesMode Blank ->
                    Time.every blankTime (always <| SetMode <| BoxesMode Letters)
                BoxesMode Letters ->
                    Time.every lettersTime (always <| SetMode <| BoxesMode Waiting)
                BoxesMode Waiting ->
                    Time.every (Time.millisecond * model.waitingTime) (always <| SetMode <| BoxesMode Guess)
                BoxesMode Guess -> 
                    Keyboard.presses (\kc -> KeyPress <| Char.fromCode kc)
                ResultMode ->
                    Keyboard.presses (\kc -> KeyPress <| Char.fromCode kc)
                SettingsMode ->
                    Keyboard.presses (\kc -> KeyPress <| Char.fromCode kc)
                StatsMode ->
                    Keyboard.presses (\kc -> KeyPress <| Char.fromCode kc)
    in
        Sub.batch [modeSubscription]
    