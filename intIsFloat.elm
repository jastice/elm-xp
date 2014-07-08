import Mouse
import Window

main = scene <~ Window.dimensions
--main = scene (100,100)
--main = collage 100 100 [ ngon 3 30 |> outlined (solid blue) ]


scene: (Int,Int) -> Element
scene (w,h) = collage w h [ circle (toFloat (min w h)/2) |> outlined (solid blue) ]

--scene2: Int -> Int -> Element
--scene2 w h = collage 100 100 [ ngon 3 (toFloat h) |> outlined (solid blue) ]

--derp: Int -> 

--min2: Int -> Int -> Int
--min2 a b = min a b