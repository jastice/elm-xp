import Window

-- underscores so we don't overwrite functions accidentally
-- pos is relative to bottom center of container
type Bubble = { size_: Float, color_: Color, pos: (Float,Float) } 
aBubble = { size_=40, color_ = red, pos = (0,0) }

type Vec2 = (Float,Float)
type Force = Bubble -> Bubble

-- tuple adding, actually
plus: Vec2 -> Vec2 -> Vec2
plus (a0,b0) (a1,b1) = (a0+a1,b0+b1)

-- global upward/sideward force
buoancy: Float -> Force
buoancy t b = 
  let s = b.size_
      (fx,fy) = (sin (t/600)*2, 5)      
      x = fx * (100 / s) -- little bubbles are moved more by sideways force
      y = fy * (s / 100) -- big bubbles move up faster
  in {b | pos <- plus b.pos (x,y)}

-- combine forces of movement, applied to a bubble
forces: [Force] -> Force
forces fs b = foldl (\f b -> f b) b fs

-- just the drawing
scene: (Int,Int) -> [Bubble] -> Element
scene (w,h) bubbles = 
  let toPos (x,y) = (x, y - toFloat h/2)
      drawBubble {size_,color_,pos} = circle size_ |> filled color_ |> move (toPos pos)

  in collage w h (map drawBubble bubbles)


initBubbles = [
  { size_=40, color_=red, pos=(0,0) },
  { size_=70, color_=yellow, pos=(100,10) },
  { size_=30, color_=blue, pos=(200,10) },
  { size_=30, color_=green, pos=(-330,-5) },
  { size_=20, color_=lightBlue, pos=(-130,0) }
  ]

-- accumulated fps signal
fpst n = foldp (+) 0 (fps n)

-- takes bubbles, time, window dimensions, and updates bubbles
floatingBubbles: (Float, (Int,Int)) -> [Bubble] -> [Bubble]
floatingBubbles (t, (w,h)) init = 
  let
    h0 = toFloat h
    -- box limiting force
    limitForce b = case b.pos of
      (x,y) -> { b | pos <- plus b.pos (0, if (y+b.size_)>h0 then h0-(y+b.size_) else 0) }

    force: Float -> Force
    force t = forces [buoancy t, limitForce]

    updateBubbles: Float -> [Bubble] -> [Bubble]
    --updateBubbles t bubbles = map (id) bubbles
    updateBubbles t bubbles = map (force t) bubbles

  in
    updateBubbles t init

inSignals: Signal (Float,(Int,Int))
inSignals = (,) <~ (fpst 40) ~ Window.dimensions

main = scene <~ Window.dimensions ~ (foldp floatingBubbles initBubbles inSignals)


