module Grid exposing (Grid, CellStatus(..), Row, Cell, uncoverAll, create, isBombed, noneUncovered, rowMap, cellMap, countNeighborMines, plantBombs, get, flag, neighborClear, isWin, unclearedMineCount)

----------------------------------------
-- imports

import Array exposing (Array)
import Queue exposing (Queue)
import Set exposing (Set)
--import Debug exposing (log)
import BombIndex exposing (BombIndex)
import Random exposing (Generator)

----------------------------------------
-- types

type alias Grid =
  Array Row

type alias Row =
  Array Cell

type alias Cell =
  { status : CellStatus
  , hasBomb : Bool
  }

type alias CellYX = (Cell, Int, Int)

type CellStatus
  = Cleared
  | Flagged
  | Covered

----------------------------------------
-- functions

cellMap : (Int -> Cell -> a) -> Row -> List a
cellMap fn row =
  mapc fn row

countNeighborMines : Int -> Int -> Grid -> Int
countNeighborMines y x grid =
  let
    neighbors = getNeighbors y x grid
    foldFn = (\(cell, y, x) tally -> if cell.hasBomb then tally + 1 else tally)
  in
    List.foldl foldFn 0 neighbors

create : Int -> Int -> Grid
create yLen xLen =
  Array.initialize yLen (\y -> createRow y xLen)

flag : Int -> Int -> Grid -> Grid
flag y x grid =
  case get y x grid of
    Nothing -> grid
    Just cell ->
      case cell.status of
        Flagged ->
          setStatus y x Covered grid
        Covered ->
          setStatus y x Flagged grid
        Cleared ->
          grid

get : Int -> Int -> Grid -> Maybe Cell
get y x grid =
  case Array.get y grid of
    Just row ->
      Array.get x row
    Nothing ->
      Nothing

isBombed : Grid -> Bool
isBombed grid =
  not (every (\cell -> cell.status /= Cleared || not cell.hasBomb) grid)

isWin : Grid -> Bool
isWin grid =
  let
    isAccountedFor = (\cell -> (cell.status == Cleared && not cell.hasBomb) || (cell.status /= Cleared && cell.hasBomb))
  in
    every isAccountedFor grid

neighborClear : Int -> Int -> Grid -> Grid
neighborClear y x grid =
  -- if not cleared, bail
  -- if no neighboring mines, bail
  -- if neighboring flag count not equal number
  -- clear all non-flagged, uncovered neighboring cells
  if
    isCleared y x grid
    && hasNeighborMines y x grid
    && (neighborFlagCount y x grid) == (countNeighborMines y x grid)
  then
    let
      neighbors = getNeighbors y x grid
    in
      clearAllList neighbors grid
  else
    grid

noneUncovered : Grid -> Bool
noneUncovered grid =
  every (\cell -> cell.status == Covered) grid

plantBombs : Int -> Int -> Int -> Grid -> Generator Grid
plantBombs yOrigin xOrigin bombCount grid =
  let
    rowCount = height grid
    colCount = width grid
    bombGen = BombIndex.create rowCount colCount bombCount
  in
    Random.map (\bombIndex -> plantBombsRecurs bombIndex grid) bombGen

rowMap : (Int -> Row -> a) -> Grid -> List a
rowMap fn grid =
  mapc fn grid

unclearedMineCount : Grid -> Int
unclearedMineCount grid =
  Array.foldl unclearedReducer 0 grid

uncoverAll : Int -> Int -> Grid -> Grid
uncoverAll y x grid =
  if isFlagged y x grid then
    grid
  else
    let
      newGrid = uncover y x grid
      fn = (\(cell, yy, xx) -> bordersEmpty yy xx newGrid)
      contiguous = getContiguous y x newGrid
    in
      uncoverAllRecurs y x contiguous newGrid

----------------------------------------
-- helper functions

addVisits : List CellYX -> Set (Int, Int) -> Set (Int, Int)
addVisits cells visited =
  let
    coords = List.map (\(cell, y, x) -> (y, x)) cells
  in
    visited |> Set.toList |> List.append coords |> Set.fromList

balloon : Int -> Grid -> List CellYX -> List CellYX
balloon wid grid cells =
  let
    fn = (\(cell, y, x) -> getNeighbors y x grid)
    concatted = List.concatMap fn cells
    -- if there was just one cell, then our concatMap will
    -- have failed to add that single cell to the results
    -- hence the below line.
    appended = if List.length cells == 1 then List.append cells concatted else concatted
  in
    dedupe appended [] Set.empty

bordersEmpty : Int -> Int -> Grid -> Bool
bordersEmpty y x grid =
  let
    notSelfAdjacent = notAdjacentToBomb y x grid
    neighbors = getNeighbors y x grid
    anyFn = (\(neighbor, yy, xx) -> notAdjacentToBomb yy xx grid)
    bordersNonAdjacent = List.any anyFn neighbors
  in
    notSelfAdjacent || bordersNonAdjacent

clearAllList : List (Cell, Int, Int) -> Grid -> Grid
clearAllList neighbors grid =
  case neighbors of
    [] -> grid
    (cell, y, x) :: tail ->
      let
        newGrid = uncoverAll y x grid
      in
        if isBombed newGrid then
          newGrid
        else
          clearAllList tail newGrid

createCell : Int -> Int -> Cell
createCell y x =
  Cell Covered False

createRow : Int -> Int -> Row
createRow y xLen =
  Array.initialize xLen (\x -> createCell y x)

dedupe : List CellYX -> List CellYX -> Set (Int, Int) -> List CellYX
dedupe dupes noDupes deduper =
  case dupes of
    [] -> noDupes
    (cell, y, x) :: remainingDupes ->
      let
        isDupe = Set.member (y, x) deduper
        newDeduper = if isDupe then deduper else Set.insert (y, x) deduper
        cellYX = (cell, y, x)
        newNoDupes = if isDupe then noDupes else cellYX :: noDupes
      in
        dedupe remainingDupes newNoDupes newDeduper

every : (Cell -> Bool) -> Grid -> Bool
every fn grid =
  Array.foldl (\row soFar -> soFar && (everyInRow fn row)) True grid

everyInRow : (Cell -> Bool) -> Row -> Bool
everyInRow fn row =
  Array.foldl (\cell soFar -> soFar && (fn cell)) True row

find : (a -> Bool) -> List a -> Maybe a
find fn list =
  case list of
    a :: rest ->
      if fn a then
        Just a
      else
        find fn rest
    [] ->
      Nothing

getContiguous : Int -> Int -> Grid -> List CellYX
getContiguous y x grid =
  case get y x grid of
    Nothing -> []
    Just cell ->
      let
        originCellYX = (cell, y, x)
      in
        case getZeroCell y x grid of
          Nothing -> [originCellYX]
          Just (zCell, yz, xz) ->
            let
              queue = Queue.singleton (zCell, yz, xz)
              wid = width grid
              visited = Set.singleton (yz, xz)
              results = []
            in
              getContiguousRecurs queue visited results grid

getContiguousRecurs : Queue CellYX -> Set (Int, Int) -> List CellYX -> Grid -> List CellYX
getContiguousRecurs queue visited results grid =
  let
    (cellyx, newQueue) = Queue.deq queue
  in
    case cellyx of
      Nothing ->
        balloon (width grid) grid results
      Just (cell, y, x) ->
        let
          wid = width grid
          neighbors = getNeighbors y x grid
          filterFn = (\(a, yy, xx) -> not (Set.member (yy, xx) visited))
          unvisitedNeighbors = List.filter filterFn neighbors
          sdfsd = List.map (\(a, y, x) -> (y, x)) unvisitedNeighbors
          cellYX = (cell, y, x)
          isMatch = notAdjacentToBomb y x grid
          nextVisited = if isMatch then addVisits unvisitedNeighbors visited else visited
          nextQueue = if isMatch then Queue.enqAll unvisitedNeighbors newQueue else newQueue
          nextResults = if isMatch then cellYX :: results else results
        in
          getContiguousRecurs nextQueue nextVisited nextResults grid

getNeighbors : Int -> Int -> Grid -> List CellYX
getNeighbors y x grid =
  let
    yUp = y+1
    yDn = y-1
    xUp = x+1
    xDn = x-1
    list =
      [ getWithCoords yDn xDn grid
      , getWithCoords yDn x   grid
      , getWithCoords yDn xUp grid
      , getWithCoords y   xUp grid
      , getWithCoords yUp xUp grid
      , getWithCoords yUp x   grid
      , getWithCoords yUp xDn grid
      , getWithCoords y   xDn grid
      ]
  in
    List.filterMap identity list

getWithCoords : Int -> Int -> Grid -> Maybe CellYX
getWithCoords y x grid =
  case get y x grid of
    Nothing -> Nothing
    Just cell -> Just (cell, y, x)

getZeroCell : Int -> Int -> Grid -> Maybe CellYX
getZeroCell y x grid =
  case get y x grid of
    Nothing -> Nothing
    Just cell ->
      if notAdjacentToBomb y x grid then
        Just (cell, y, x)
      else
        let
          neighbors = getNeighbors y x grid
          findFn = (\(cell, yy, xx) -> notAdjacentToBomb yy xx grid)
        in
          find findFn neighbors

hasNeighborFlags : Int -> Int -> Grid -> Bool
hasNeighborFlags y x grid =
  let
    neighbors = getNeighbors y x grid
    fn = (\(cell, y, x) -> cell.status == Flagged)
  in
    List.any fn neighbors

hasNeighborMines : Int -> Int -> Grid -> Bool
hasNeighborMines y x grid =
  (countNeighborMines y x grid) > 0

height : Grid -> Int
height grid =
  Array.length grid

isCleared : Int -> Int -> Grid -> Bool
isCleared y x grid =
  case get y x grid of
    Nothing -> False
    Just cell ->
      case cell.status of
        Flagged -> False
        Covered -> False
        Cleared -> True

isFlagged : Int -> Int -> Grid -> Bool
isFlagged y x grid =
  case get y x grid of
    Nothing -> False
    Just cell ->
      case cell.status of
        Flagged -> True
        _ -> False

mapWithCounter : Int -> Array b -> (Int -> a -> b) -> Array a -> Array b
mapWithCounter count newArr fn oldArr =
  let (item, oldArrX) =
    shift oldArr
  in
    case item of
      Just a ->
        let newArrX =
          Array.push (fn count a) newArr
        in
          mapWithCounter (count + 1) newArrX fn oldArrX
      Nothing ->
        newArr

mapc : (Int -> a -> b) -> Array a -> List b
mapc fn arr =
  Array.toList (mapWithCounter 0 Array.empty fn arr)

neighborFlagCount : Int -> Int -> Grid -> Int
neighborFlagCount y x grid =
  let
    neighbors = getNeighbors y x grid
    fn = (\(cell, y, x) tally -> if cell.status == Flagged then tally + 1 else tally)
  in
    List.foldl fn 0 neighbors

notAdjacentToBomb : Int -> Int -> Grid -> Bool
notAdjacentToBomb y x grid =
  case get y x grid of
    Nothing -> True
    Just cell ->
      (not cell.hasBomb) && ((countNeighborMines y x grid) == 0)

plantBombsRecurs : BombIndex -> Grid -> Grid
plantBombsRecurs bombIndex grid =
  case bombIndex of
    [] -> grid
    (isBomb, y, x) :: rest ->
      let
        newGrid = setBomb y x isBomb grid
      in
        plantBombsRecurs rest newGrid

setBomb : Int -> Int -> Bool -> Grid -> Grid
setBomb y x isBomb grid =
  case Array.get y grid of
    Just row ->
      Array.set y (setBombInRow x isBomb row) grid
    Nothing ->
      grid

setBombInRow : Int -> Bool -> Row -> Row
setBombInRow x isBomb row =
  case Array.get x row of
    Just cell ->
      Array.set x { cell | hasBomb = isBomb } row
    Nothing ->
      row

setStatus : Int -> Int -> CellStatus -> Grid -> Grid
setStatus y x status grid =
  case Array.get y grid of
    Just row ->
      Array.set y (setStatusInRow x status row) grid
    Nothing ->
      grid

setStatusInRow : Int -> CellStatus -> Row -> Row
setStatusInRow x status row =
  case Array.get x row of
    Just cell ->
      Array.set x { cell | status = status } row
    Nothing ->
      row

shift : Array a -> (Maybe a, Array a)
shift arr =
  case Array.get 0 arr of
    Just a ->
      (Just a, Array.slice 1 (Array.length arr) arr)
    Nothing ->
      (Nothing, arr)

unclearedReducer : Row -> Int -> Int
unclearedReducer row tally =
  Array.foldl unclearedRowReducer tally row

unclearedRowReducer : Cell -> Int -> Int
unclearedRowReducer cell tally =
  if cell.status == Covered && cell.hasBomb then
    tally + 1
  else
    tally

uncover : Int -> Int -> Grid -> Grid
uncover y x grid =
  case get y x grid of
    Just cell ->
      setStatus y x Cleared grid
    Nothing ->
      grid

uncoverAllRecurs : Int -> Int -> List CellYX -> Grid -> Grid
uncoverAllRecurs y x contiguous grid =
  case contiguous of
    cellyx :: rest ->
      let
        (cell, yy, xx) = cellyx
        newGrid = uncover yy xx grid
      in
        uncoverAllRecurs yy xx rest newGrid
    [] ->
      grid

width : Grid -> Int
width grid =
  case Array.get 0 grid of
    Nothing ->
      0
    Just row ->
      Array.length row
