type alias State = { x: Float }
type Action = Key Int | Whatever

updateState: Action -> State -> State
updateState action state = 
  case action of
    Key key -> 
      if | key == 0 -> { state | z = 2 }
         | otherwise -> state
    _ -> state
