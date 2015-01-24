type Body a = { a | pos:(Int,Int) }
type Bubble = Body { radius: Int }
type Box = Body { w: Int, h: Int }

data WBody = WBox Box | WBubble Bubble

mv: (Int,Int) -> Body a -> Body a
mv (x,y) body =
  let (x0,y0)= body.pos
  in { body | pos <- (x0+x, y0+y)}

mvWBody: (Int,Int) -> WBody -> WBody
mvWBody vec wrap = 
  case wrap of
    WBox body -> WBox (mv vec body)
    WBubble body -> WBubble (mv vec body)

-- apply a function to the body wrapped in a WBody
applyWBody: (Body a -> Body a) -> WBody -> WBody
applyWBody f wrap = case wrap of
  WBox body -> WBox (f body)
  --WBubble body -> WBubble (f body)
  WBubble body -> myBox


myBubble = WBubble { pos=(0,0), radius=10 }
myBox = WBox { pos=(10,10), w=5, h=5 }
bodies = [myBox,myBubble]

--main = asText (map (applyWBody <| mv (1,1)) bodies)

