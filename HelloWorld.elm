import Mouse

main =
  scene . truncate . inMilliseconds <~ foldp (+) 0 (fpsWhen 40 Mouse.isDown)

--scene: Float -> Form
scene t = 
  let elem (x,y) s r txt = move (x,y) <| scale ((cos <| toFloat t/s)*2) <| (rotated txt) <| (toFloat t) / r
  in collage 600 600 [
    elem (-50,-50) 200 1000 "Spin me round baby right round",
    elem (100,150) 400 2000 "like a record baby"
  ]

rotated txt x = rotate (turns x) <| toForm <| plainText txt

pyt x y = sqrt (x*x + y*y)