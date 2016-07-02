module Queue exposing (Queue, singleton, enq, deq, enqAll, peek, length, toList)

type Queue a
  = Empty
  | Mono a
  | Multi a (Queue a) a

singleton : a -> Queue a
singleton a =
  Mono a

enq : a -> Queue a -> Queue a
enq val queue =
  case queue of
    Empty ->
      Mono val
    Mono item ->
      Multi item Empty val
    Multi front midQueue back ->
      Multi front (enq back midQueue) val

enqAll : List a -> Queue a -> Queue a
enqAll things queue =
  case things of
    head :: tail ->
      enqAll tail (enq head queue)
    [] ->
      queue

deq : Queue a -> (Maybe a, Queue a)
deq queue =
  case queue of
    Empty ->
      (Nothing, Empty)
    Mono item ->
      (Just item, Empty)
    Multi front midQueue back ->
      let
        (maybeNewFront, newMidQueue) = deq midQueue
      in
        case maybeNewFront of
          Nothing ->
            (Just front, Mono back)
          Just newFront ->
            (Just front, Multi newFront newMidQueue back)

peek : Queue a -> Maybe a
peek queue =
  case queue of
    Empty ->
      Nothing
    Mono item ->
      Just item
    Multi front midQueue back ->
      Just front

length : Queue a -> Int
length queue =
  case queue of
    Empty ->
      0
    Mono item ->
      1
    Multi front midQueue back ->
      2 + (length midQueue)

toList : Queue a -> List a
toList queue =
  case queue of
    Empty ->
      []
    Mono item ->
      [item]
    Multi front midQueue back ->
      List.append (front :: (toList midQueue)) [back]
