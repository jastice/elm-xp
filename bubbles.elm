import Window

-- underscores so we don't overwrite functions accidentally
-- pos is relative to bottom center of container
type Bubble = { size_: Float, color_: Color, pos: (Float,Float) } 
aBubble = { size_=40, color_ = red, pos = (0,0) }

applyForce: (Float,Float) -> Bubble -> Bubble
applyForce (fx,fy) b = 
  let (x0,y0) = b.pos
      x = fx * (100 / b.size_) -- little bubbles bouy faster
      y = fy * (b.size_ / 100) -- big bubbles move up faster
  in { b | pos <- (x0+x,y0+y)}

-- x/y forces of movement, dependent on time
force: Float -> (Float,Float)
force t = (sin (t/600)*2, 2)

-- accumulated fps
fpst n = foldp (+) 0 (fps n)

floatingBubbles: [Bubble] -> Signal [Bubble]
floatingBubbles bubbles = 
  foldp (\t -> \b -> map (applyForce (force t)) b) bubbles (fpst 20)

scene: (Int,Int) -> [Bubble] -> Element
scene (w,h) bubbles = 
  let toPos (x,y) = (x, y - toFloat h/2)
      drawBubble {size_,color_,pos} = circle size_ |> filled color_ |> move (toPos pos)

  in collage w h (map drawBubble bubbles)

initBubbles = [
  { size_=40, color_=red, pos = (0,0) },
  { size_=70, color_=yellow, pos=(100,10) },
  { size_=30, color_=blue, pos=(200,10) },
  { size_=30, color_=green, pos=(-330,-5) },
  { size_=20, color_=green, pos=(-130,0) }
  ]

main = scene <~ Window.dimensions ~ floatingBubbles initBubbles