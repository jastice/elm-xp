
import Mouse

scene x = 
  let vanilla1 = square 100 |> textured "vanilla.jpeg" |> move (x,0)
      vanilla2 = square 66 |> textured "vanilla.jpeg" |> move (0,200-x)
  in collage 800 800 [vanilla1,vanilla2]

main = scene . toFloat <~ count Mouse.clicks
--main = scenestatic
--(fps 40)
  

    --import Mouse

    --scene x = 
    --  let vanilla1 = circle 100 |> textured "vanilla.jpeg" |> move (x,0)
    --      vanilla2 = circle 66 |> textured "vanilla.jpeg" |> move (0,200-x)
    --  in collage 800 800 [vanilla1,vanilla2]

    --main =  toFloat . scene <~ count Mouse.clicks