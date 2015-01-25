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
## Click-and-Drag to Stamp!

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
type alias StampState = { state: Stamp, dragStart: Stamp }
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
  (Press <~ sampleOn Mouse.clicks currentStamp)

type StampAction = StampKey Int | StampDrag Dragging.DragState | StampMove (Int,Int)
-- actions affecting the stamp state
stampActions: Signal StampAction
stampActions = mergeMany [
  StampKey <~ Keyboard.lastPressed,
  StampDrag <~ Dragging.dragging,
  StampMove <~ Mouse.position
  ]

-- the actual stamp state is dependent on stamp actions and previous stamp state
stampState = foldp updateState {state=protoStamp, dragStart=protoStamp} stampActions
currentStamp = .state <~ stampState
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

toFloat2 (a,b) = (toFloat a, toFloat b)
sub2 (x0,y0) (x1,y1) = (x0-x1, y0-y1)
hyp (a,b) = sqrt ( a*a + b*b )
dist a b = hyp (b `sub2` a)


-- update state based on key pressed
updateState: StampAction -> StampState -> StampState
updateState stampAction {state,dragStart} = case stampAction of
  StampKey key ->
    let s =
      if | key >= 51 && key <= 57 -> { state | corners <- key-48 } -- 3-9
         | key == 82 -> { state | color <- myRed } -- r
         | key == 71 -> { state | color <- myGreen } -- g
         | key == 66 -> { state | color <- myBlue } -- b
         | key == 87 -> { state | size <- state.size + sizeIncrement } -- w
         | key == 83 && state.size >= 2*sizeIncrement -> { state | size <- state.size - sizeIncrement } -- s
         | key == 68 -> { state | rotation <- state.rotation - (turns rotationIncrement)} -- d
         | key == 65 -> { state | rotation <- state.rotation + (turns rotationIncrement)} -- a
         | otherwise -> state
      in StampState s s
  StampDrag (Just {start,now}) -> 
    let startf = toFloat2 start
        nowf = toFloat2 now
        (xd,yd) = nowf `sub2` startf
        current = { state | 
          size <- clamp 0 5000 <| (dist startf nowf),
          rotation <- atan2 xd yd
        }
    in StampState current dragStart
  StampMove p -> 
    let newState = { state | pos <- p }
    in StampState newState newState
  _ -> StampState state state


-- overlay instructions on the playground
scene playground = layers [ playground, instructions ]
main = scene <~ (playground <~ Window.dimensions ~ currentStamp ~ stamps)
