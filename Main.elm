import Html exposing (Html, button, div, text, td, tr, table, hr, strong, span, h1, i, strong)
import Html.App as Html
import Html.Events exposing (onClick, on, onWithOptions)
import Html.Attributes exposing (style, title, class, classList, attribute, href)
import Grid exposing (Grid, Row, Cell)
import Random exposing (generate)
import Debug exposing (log)
import Json.Decode as Decode exposing (Decoder, (:=))
import String
import Time exposing (Time, second)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
  { secondsElapsed : Int
  , isCounting : Bool
  , grid : Grid
  }

init : (Model, Cmd Msg)
init =
  ( Model 0 False (Grid.create gridHeight gridWidth)
  , Cmd.none
  )


gridHeight = 16
gridWidth = 30
bombCount = 99

-- UPDATE


type Msg
  = Flag Int Int
  | Plant Int Int
  | SetPlanted Grid Int Int
  | Clear Int Int
  | NeighborClear Int Int
  | Restart
  | Tick Time
  | None

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
      None -> (model, Cmd.none)
      Tick newTime ->
        if model.isCounting then
          ({ model | secondsElapsed = model.secondsElapsed + 1 }, Cmd.none)
        else
          (model, Cmd.none)
      Restart ->
        (Model 0 False (Grid.create gridHeight gridWidth), Cmd.none)
      Flag y x ->
        ({ model | grid = Grid.flag y x model.grid }, Cmd.none)
      Plant y x ->
        (model, Random.generate (\newGrid -> SetPlanted newGrid y x) (Grid.plantBombs y x bombCount model.grid))
      SetPlanted newGrid y x ->
        case Grid.get y x newGrid of
          Nothing -> (model, Cmd.none)
          Just cell ->
            if cell.hasBomb then
              (model, Random.generate (\yetAnotherGrid -> SetPlanted yetAnotherGrid y x) (Grid.plantBombs y x bombCount model.grid))
            else
              ({ model | grid = Grid.uncoverAll y x newGrid, isCounting = True }, Cmd.none)
      Clear y x ->
        let
          newGrid = Grid.uncoverAll y x model.grid
          isBombed = Grid.isBombed newGrid
          isWin = Grid.isWin newGrid
          isCounting = not (isBombed || isWin)
        in
          ({ model | grid = newGrid, isCounting = isCounting }, Cmd.none)
      NeighborClear y x ->
        ({ model | grid = Grid.neighborClear y x model.grid }, Cmd.none)

-- VIEW


view : Model -> Html Msg
view { secondsElapsed, grid } =
  let
    isBombed = Grid.isBombed grid
    isWin = Grid.isWin grid
    noneUncovered = Grid.noneUncovered grid
    remainingCount = Grid.unclearedMineCount grid
  in
    div []
      [ h1 [] [text "ELMSWEEPER"]
        , div [class "grid-wrapper"]
        [ div [class "grid-head"]
          [ span [class "grid-remaining"] [ text (leftPad "0" 3 (remaining isWin remainingCount)) ]
          , span [class "grid-time"] [ text (leftPadMax "0" 3 secondsElapsed 999) ]
          , face isBombed isWin
          ]
        , tgrid isBombed noneUncovered isWin grid
        ]
      , Html.p []
        [ text "Github: "
        , Html.a [Html.Attributes.href "https://github.com/greim/elmsweeper"] [text "github.com/greim/elmsweeper"]
        ]
        , Html.p []
        [ Html.a
          [ href "https://twitter.com/share"
          , class "twitter-share-button"
          , attribute "data-url" "https://greim.github.io/elmsweeper/"
          , attribute "data-text" "Elmsweeper: Minesweeper in Elm."
          , attribute "data-via" "greim"
          , attribute "data-size" "large"
          , attribute "data-hashtags" "elmlang"
          ] [text "Tweet"]
        ]
      ]

face isBombed isWin =
  if isWin then
    span [class "face win", onClick Restart] []
  else if isBombed then
    span [class "face sad", onClick Restart] []
  else
    span [class "face happy", onClick Restart] []

remaining : Bool -> Int -> String
remaining isWin remaining =
  if isWin then
    "0"
  else if remaining == 0 then
    toString bombCount
  else
    toString remaining

leftPadMax : String -> Int -> Int -> Int -> String
leftPadMax padder width n maximum =
  let
    maxxed = toString (min n maximum)
  in
    leftPad padder width maxxed

leftPad : String -> Int -> String -> String
leftPad padder width str =
  if String.length str >= width then
    str
  else
    leftPad padder width (padder ++ str)

statusText : Bool -> Bool -> Int -> String
statusText isBombed isWin remaining =
  if isBombed then
    "Lose!"
  else if isWin then
    "Win!"
  else if remaining == 0 then
    "Remaining: " ++ (toString bombCount)
  else
    "Remaining: " ++ (toString remaining)

tgrid : Bool -> Bool -> Bool -> Grid -> Html Msg
tgrid isBombed noneUncovered isWin grid =
  table [class "grid"]
    (Grid.rowMap (trow isBombed noneUncovered isWin grid) grid)

trow : Bool -> Bool -> Bool -> Grid -> Int -> Row -> Html Msg
trow isBombed noneUncovered isWin grid y row =
  tr []
    (Grid.cellMap (tcell isBombed noneUncovered isWin grid y) row)

tcell : Bool -> Bool -> Bool -> Grid -> Int -> Int -> Cell -> Html Msg
tcell isBombed noneUncovered isWin grid y x cell =
  let
    classes = case cell.status of
      Grid.Cleared -> if cell.hasBomb then cellBombedClasses else cellClearedClasses
      Grid.Flagged -> cellFlaggedClasses
      Grid.Covered -> if isBombed && cell.hasBomb then cellRevealBombClasses else cellCoveredClasses
  in
    td
      [ classes
      , onCellClick2 isBombed noneUncovered isWin y x cell
      , killContext
      ]
      [ getCellContents cell (Grid.countNeighborMines y x grid) isBombed
      ]

killContext : Html.Attribute Msg
killContext =
  onWithOptions "contextmenu" { stopPropagation = False, preventDefault = True } (Decode.succeed None)


onCellClick2 : Bool -> Bool ->  Bool -> Int -> Int -> Cell -> Html.Attribute Msg
onCellClick2 isBombed noneUncovered isWin y x cell =
  if noneUncovered then
    onClick (Plant y x)
  else
    if isBombed || isWin then
      onClick None
    else
      on "mousedown" (decodeCellClickEvent y x)

decodeCellClickEvent : Int -> Int -> Decoder Msg
decodeCellClickEvent y x =
  ("buttons" := Decode.int) |> (buttonInfo y x)

buttonInfo : Int -> Int -> Decoder Int -> Decoder Msg
buttonInfo y x evDecoder =
  Decode.customDecoder evDecoder (handleButton y x)

handleButton : Int -> Int -> Int -> Result String Msg
handleButton y x evButtons =
  case evButtons of
    2 -> Ok (Flag y x)
    3 -> Ok (NeighborClear y x)
    _ -> Ok (Clear y x)


cellBaseClasses = [("grid-cell",True)]
cellClearedClasses = classList (List.append [("cleared",True)] cellBaseClasses)
cellBombedClasses = classList (List.append [("bombed",True)] cellBaseClasses)
cellRevealBombClasses = classList (List.append [("covered reveal-bomb",True)] cellBaseClasses)
cellFlaggedClasses = classList (List.append [("flagged",True)] cellBaseClasses)
cellCoveredClasses = classList (List.append [("covered",True)] cellBaseClasses)

getCellContents : Cell -> Int -> Bool -> Html Msg
getCellContents cell count isBombed =
  if cell.status == Grid.Cleared && count > 0 then
    strong [class ("number n" ++ (toString count))] [text (toString count)]
  else
    text ""

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick
