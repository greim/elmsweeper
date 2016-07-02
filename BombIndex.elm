module BombIndex exposing (BombIndex, create)

import Array exposing (Array)
import Random exposing (Generator)
import Random.Array exposing (shuffle)
--import Debug exposing (log)

type alias BombIndex = List (Bool, Int, Int)
type alias RawBombIndex = Array Bool

create : Int -> Int -> Int -> Generator BombIndex
create rowCount colCount bombCount =
  let
    unshuffled = unshuffledBombs (rowCount * colCount) bombCount
    shuffler = shuffle unshuffled
  in
    Random.map (\shuffled -> annotate colCount shuffled) shuffler

unshuffledBombs : Int -> Int -> RawBombIndex
unshuffledBombs indexLen bombCount =
  let
    limitedBombCount = if bombCount < indexLen then bombCount else indexLen - 1
  in
    Array.initialize indexLen (\idx -> idx < limitedBombCount)

annotate : Int -> RawBombIndex -> BombIndex
annotate colCount bombIndex =
  bombIndex
    |> Array.toIndexedList
    |> List.map (\(idx, isBomb) -> (isBomb, idx // colCount, idx % colCount))
