type Body s = { pos:(Int,Int), shape : s }
 
data Shape = Box {w:Int, h:Int} | Bubble {radius:Int}
 
mv: (Int,Int) -> Body a -> Body a
mv (x,y) body =
  let (x0,y0)= body.pos
  in { body | pos <- (x0+x, y0+y)}
 
--myBubble = Body (0,0) (Bubble {radius=10})
myBubble = {pos=(0,0), shape = Bubble {radius=10}}
myBox = Body (10,10) <| Box {w=5, h=5}
bodies = [myBox,myBubble]
 
main = asText (map (mv (1,1)) bodies)