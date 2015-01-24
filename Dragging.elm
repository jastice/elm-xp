module Dragging where

import Keyboard
import Mouse
import Time
import Signal (..)
import Text (asText)


--main = asText <~ count Mouse.clicks
main = asText <~ dragging

{-| Starting and current position of a drag. Nothing when there is no drag.
-}
type alias DragState = Maybe {start: (Int,Int), now: (Int,Int)}

type alias MouseState = {dragging: Bool, pos: (Int,Int)}

updateDragState: MouseState -> DragState -> DragState
updateDragState mouse state = 
  if not mouse.dragging then Nothing else
  case state of
    Nothing -> Just { start = mouse.pos, now = mouse.pos}
    Just s -> Just { s | now <- mouse.pos }

updateMouseState: Bool -> (Int,Int) -> MouseState
updateMouseState dragging pos = {dragging=dragging, pos=pos}

mouseState: Signal MouseState
mouseState = updateMouseState <~ Mouse.isDown ~ Mouse.position

{-| Signal of starting and current position of a basic drag. Nothing when there is no drag.
-}
dragging: Signal DragState
dragging = dropRepeats (foldp updateDragState Nothing mouseState)

--dragWhen: Signal Bool -> Signal DragState
--dragWhen when = 
--  let ms = keepWhen when False Mouse.position
--  in

--dragOn: Keyboard.KeyCode -> Signal DragState
--dragOn key = keepWhen (Keyboard.isDown key) 
