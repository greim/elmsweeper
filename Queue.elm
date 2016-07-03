module Queue exposing (Queue, singleton, enq, deq, enqAll, length)

type alias Queue a =
  { inq : List a
  , outq : List a
  }

singleton : a -> Queue a
singleton a =
  Queue [] [a]

enq : a -> Queue a -> Queue a
enq val queue =
  { queue | inq = val :: queue.inq }

enqAll : List a -> Queue a -> Queue a
enqAll things queue =
  { queue | inq = List.append things queue.inq }

deq : Queue a -> (Maybe a, Queue a)
deq queue =
  case queue.outq of
    [] ->
      case List.reverse queue.inq of
        val :: outq ->
          (Just val, Queue [] outq)
        [] ->
          (Nothing, queue)
    val :: outq ->
      (Just val, { queue | outq = outq })

length : Queue a -> Int
length queue =
  (List.length queue.inq) + (List.length queue.outq)
