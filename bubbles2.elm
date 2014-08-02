import BubblePhysics

-- shares radius and pos with Bubble
type SceneBubble = { radius: Float, pos: (Float,Float), color_: Color }

toSceneBubble: Color -> Bubble -> SceneBubble
toSceneBubble color_ b = { b | color_=color_}

-- translate within (w,h) window an absolute (x,y) position relative to the centered (0,0) origin
toPos: (Float,Float) -> (Float,Float) -> (Float,Float)
toPos (w,h) (x,y) = (x - w/2, h/2 - y)

drawBubble: SceneBubble -> Element
drawBubble {radius, color_, pos} = circle radius |> filled color_ |> move (toPos pos)

main = asText "compiled!"