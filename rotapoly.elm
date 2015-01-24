getSpinning : Float -> Element
getSpinning ms = 
  let rot = turns (ms / 10000)
  in collage 400 400 
    [
    move (180,100) <| toForm (asText <| turns <| rot - (toFloat <| floor rot)),
    move (-100,100) <| toForm (asText <| ms),
    rotate rot (filled blue (ngon 7 100))
    ]


main = getSpinning <~ (foldp (+) 0 (fps 20))