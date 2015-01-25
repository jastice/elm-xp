import Mouse
import Keyboard
import Window
import Dragging
import Markdown
import Color (..)
import List
import Signal (Signal,(<~), (~), foldp, merge, mergeMany, sampleOn)
import Graphics.Collage (..)
import Graphics.Element (..)
import Keyboard.Keys as K

instructions = Markdown.toElement """
## Click to stamp!

    Keys:
      3-9 stamp shape (corners)
      r   red stamp!
      g   green stamp!
      b   blue stamp!
      w/s resize stamp
      a/d rotate stamp
      u   undo
"""

type alias Stamp = { pos: (Int,Int), corners: Int, size: Float, rotation: Float, color: Color }
protoStamp = { pos=(0,0), corners=3, size=30, rotation=0, color=myGreen }

type Action = Press Stamp | Undo | Inaction

-- update stamp history based on actions
updateHistory: Action -> List Stamp -> List Stamp
updateHistory action history = case action of
  Press stamp -> stamp :: history
  Undo -> List.drop 1 history
  Inaction -> history

keyAction k = if k == 85 then Undo else Inaction

-- actions affecting the board
actions: Signal Action
actions = merge
  (keyAction <~ Keyboard.lastPressed)
  (Press <~ sampleOn Mouse.clicks stampState)

type StampAction = StampKey Int | StampDrag Dragging.DragState | StampMove (Int,Int)
-- actions affecting the stamp state
stampActions: Signal StampAction
stampActions = mergeMany [
  StampKey <~ Keyboard.lastPressed,
  StampDrag <~ Dragging.dragging,
  StampMove <~ Mouse.position
  ]

-- the actual stamp state is dependent on stamp actions and previous stamp state
stampState = foldp updateState protoStamp stampActions
stamps = foldp updateHistory [] actions


playground (w,h) currentStamp stamps =
  let toPos (x,y) = (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)
      printStamp {pos, corners, size, rotation, color} = 
        ngon corners size |> filled color |> rotate rotation |> move (toPos pos)
  in collage w h <| printStamp currentStamp :: List.map printStamp stamps

myColor h = hsla (turns h) 0.5 0.3 0.8
myRed = myColor 0
myGreen = myColor 0.4
myBlue = myColor 0.6
sizeIncrement = 5
rotationIncrement = 1.0/32

-- good ol' pythagoras
hyp: Int -> Int -> Float
hyp a b = 
  let a1 = toFloat a
      b1 = toFloat b
  in sqrt (a1*a1 + b1*b1)
-- vector distance
dist (x1,y1) (x2,y2) = hyp (x2-x1) (y2-y1)

-- update state based on key pressed
updateState: StampAction -> Stamp -> Stamp
updateState stampAction state = case stampAction of
  StampKey key ->
    if | key >= 51 && key <= 57 -> { state | corners <- key-48 } -- 3-9
       | key == 82 -> { state | color <- myRed } -- r
       | key == 71 -> { state | color <- myGreen } -- g
       | key == 66 -> { state | color <- myBlue } -- b
       | key == 87 -> { state | size <- state.size + sizeIncrement } -- w
       | key == 83 && state.size >= 2*sizeIncrement -> { state | size <- state.size - sizeIncrement } -- s
       | key == 68 -> { state | rotation <- state.rotation - (turns rotationIncrement)} -- d
       | key == 65 -> { state | rotation <- state.rotation + (turns rotationIncrement)} -- a
       | otherwise -> state
  StampDrag (Just {start,now}) -> { state | size <- (dist start now) }
  StampMove p -> { state | pos <- p }
  _ -> state


-- overlay instructions on the playground
scene pg = layers [ pg, instructions ]
main = scene <~ (playground <~ Window.dimensions ~ stampState ~ stamps)
