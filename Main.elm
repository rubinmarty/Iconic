import Dict exposing (Dict)
import Html exposing (Html)
import Array exposing (Array)
import Random
import Random.Extra as Random
import Random.Char as Random
import Random.Array as Random


type alias Score =
    ( Int , Int )

type Mode =
      IntroMode Int
    | DisplayMode
    | GuessMode
    | ResultMode

type alias Model =
    { data : Array Char
    , scores : Dict Int Score
    , size : Int
    , mode : Mode
    , index : Int
    }

type Msg =
      NoOp
    | TimeUp
    | Guess Char
    | ClickThru
    | NewChars
    | SetData (Array Char)

main : Program Never Model Msg
main =
    Html.program
        { init = init ! []
        , update = update
        , subscriptions = (\_ -> Sub.none)
        , view = view
        }

init : Model
init =
    { data = Array.empty
    , scores = Dict.empty
    , size = 12
    , mode = IntroMode 0
    , index = -1
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []
        TimeUp ->
            { model | mode = GuessMode } ! []
        Guess ch ->
            guess ch model ! []
        ClickThru ->
            model ! []
        NewChars ->
            model ! [Random.generate SetData (makeData model.size)]
        SetData chs ->
            { model | data = chs} ! []

view : Model -> Html Msg
view model =
    Html.text "test"


makeData : Int -> Random.Generator (Array Char)
makeData x =
    Random.array x Random.upperCaseLatin

addRight : Score -> Score
addRight ( r , w ) =
    ( r + 1 , w )

addWrong : Score -> Score
addWrong ( r , w ) =
    ( r , w + 1 )

updateScores : Dict Int Score -> Int -> Bool -> Dict Int Score
updateScores scores num isRight =
    Dict.get num scores
    |> Maybe.withDefault ( 0 , 0 )
    |> (if isRight then addRight else addWrong)
    |> (\v -> Dict.insert num v scores)

guess : Char -> Model -> Model
guess ch model =
    Array.get model.index model.data
    |> Maybe.map ((==) ch)
    |> Maybe.withDefault False
    |> updateScores model.scores model.size
    |> (\scrs -> { model | scores = scrs })