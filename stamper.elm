import Mouse
import Keyboard
import Window

{--
  Click to stamp! 
  Keys:
    3-9 stamp shape (corners)
    r   red stamp!
    t   green stamp!
    y   blue stamp!
    w/s resize stamp
    a/d rotate stamp
--}

main = lift3 scene Window.dimensions stampState stamps

type Stamp = { pos: (Int,Int), corners: Int, size: Float, rotation: Float, color: Color }
protoStamp = { pos=(0,0), corners=3, size=30, rotation=0, color=myGreen }
makeStamp: (Int,Int) -> Int -> Stamp
makeStamp pos corners = { protoStamp | pos<-pos, corners<-corners }

-- put together stamp state from last number key press and mouse position when clicking
keyState = foldp updateState protoStamp Keyboard.lastPressed
stampState = (\s p -> {s|pos<-p}) <~ keyState ~ Mouse.position
stamps = foldp (::) [] (sampleOn Mouse.clicks stampState)


scene (w,h) currentStamp stamps =
  let toPos (x,y) = (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)
      printStamp {pos, corners, size, rotation, color} = 
        ngon corners size |> filled color |> rotate rotation |> move (toPos pos)

  in collage w h <| printStamp currentStamp :: map printStamp stamps

myRed = hsla (turns 0.0) 0.5 0.3 0.8
myGreen = hsla (turns 0.4) 0.5 0.3 0.8
myBlue = hsla (turns 0.6) 0.5 0.3 0.8
sizeIncrement = 5
rotationIncrement = 1.0/32

updateState key state = 
  if | key >= 51 && key <= 57 -> { state | corners <- key-48 } -- 3-9
     | key == 82 -> { state | color <- myRed } -- r
     | key == 84 -> { state | color <- myGreen } -- t
     | key == 89 -> { state | color <- myBlue } -- y
     | key == 87 -> { state | size <- state.size + sizeIncrement } -- w
     | key == 83 && state.size >= 10 -> { state | size <- state.size - sizeIncrement } -- s
     | key == 68 -> { state | rotation = state.rotation - (turns rotationIncrement)} -- d
     | key == 65 -> { state | rotation = state.rotation + (turns rotationIncrement)} -- a
     | otherwise -> state