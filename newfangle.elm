import Mouse
import Window

--main = scene <~ Window.dimensions

main = scene <~ Window.dimensions ~ clickPositions

clickPositions = foldp (::) [] (sampleOn Mouse.clicks Mouse.position)

-- scale relative to window size
scaler: (Int,Int) -> Float
scaler (x,y) = toFloat (min x y) / 2

scene: (Int,Int) -> [(Int,Int)] -> Element
scene (w,h) ps =
    let s = scaler (w,h)
        toPos (x,y) = (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)
        mark (x,y) = ngon 3 (s/3) |> outlined (dashed red) |> move (toPos (x,y))
    in collage w h (map mark ps)
