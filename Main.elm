import Html exposing (Html, button, div, text, td, tr, table, hr, strong, span, h1, i, strong)
import Html.App as Html
import Html.Events exposing (onClick, on, onWithOptions)
import Html.Attributes exposing (style, title, class, classList)
import Grid exposing (Grid, Row, Cell)
import Random exposing (generate)
import Debug exposing (log)
import Json.Decode as Decode exposing (Decoder, (:=))
import String

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model = Grid

init : (Model, Cmd Msg)
init =
  ( Grid.create gridHeight gridWidth
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
  | None

update : Msg -> Model -> (Model, Cmd Msg)
update msg grid =
  case msg of
    None -> (grid, Cmd.none)
    Restart ->
      (Grid.create gridHeight gridWidth, Cmd.none)
    Flag y x ->
      ((Grid.flag y x grid), Cmd.none)
    Plant y x ->
      (grid, Random.generate (\newGrid -> SetPlanted newGrid y x) (Grid.plantBombs y x bombCount grid))
    SetPlanted newGrid y x ->
      case Grid.get y x newGrid of
        Nothing -> (grid, Cmd.none)
        Just cell ->
          if cell.hasBomb then
            (grid, Random.generate (\yetAnotherGrid -> SetPlanted yetAnotherGrid y x) (Grid.plantBombs y x bombCount grid))
          else
            ((Grid.uncoverAll y x newGrid), Cmd.none)
    Clear y x ->
      ((Grid.uncoverAll y x grid), Cmd.none)
    NeighborClear y x ->
      ((Grid.neighborClear y x grid), Cmd.none)

-- VIEW


view : Model -> Html Msg
view grid =
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
          , span [class "grid-time"] [ text "000" ]
          , face isBombed isWin
          ]
        , tgrid isBombed noneUncovered isWin grid
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
      Grid.Covered -> cellCoveredClasses
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










icon : String -> Html Msg
icon name =
  i [class ("fa fa-" ++ name)] []

cellBaseClasses = [("grid-cell",True)]
cellClearedClasses = classList (List.append [("cleared",True)] cellBaseClasses)
cellBombedClasses = classList (List.append [("bombed",True)] cellBaseClasses)
cellFlaggedClasses = classList (List.append [("flagged",True)] cellBaseClasses)
cellCoveredClasses = classList (List.append [("covered",True)] cellBaseClasses)

getCellContents : Cell -> Int -> Bool -> Html Msg
getCellContents cell count isBombed =
  case cell.status of
    Grid.Cleared ->
      if cell.hasBomb then
        icon "circle"
      else if count > 0 then
        strong [class ("number n" ++ (toString count))] [text (toString count)]
      else
        text ""
    Grid.Flagged ->
      text ""
    Grid.Covered ->
      if isBombed && cell.hasBomb then
        icon "circle"
      else
        text ""

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
