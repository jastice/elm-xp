import Mouse
import BubblePhysics (..)

inf = 1/0

someBubbles = [ 
  makeBubble 30 (-80,0) (1.5,0),
  makeBubble2 70 (80,0) (0,0) inf 1,
  makeBubble2 40 (0,200) (0.4,-3.0) 1 1,
  makeBubble2 90 (400,-300) (-2,0) 0.01 0,
  makeBubble2 20 (-400,-400) (3,0.2) 1 0.9,
  makeBubble2 10 (300,300) (-4,-4) 10 1,
  makeBubble 40 (400,200) (-5,-1)
  ]

drawBubble {radius,pos,velocity,inverseMass,restitution} = 
  group [
    circle radius |> filled (hsl radius (1/inverseMass) 0.5),
    segment (0,0) (mul2 velocity 5) |> traced (solid red),
    ["e = ", show restitution, "\nm = ", show (round (1/inverseMass))] |> join " "
      |> toText |> centered |> toForm |> move (0,radius+8)
    ] |> move pos 

scene bubbles = 
  let drawnBubbles = map drawBubble bubbles 
  in collage 800 800 drawnBubbles

tick = (\t -> (0,-0.0)) <~ every (30*millisecond)
tick2 = (\t -> (0,0)) <~ Mouse.clicks

main = scene <~ foldp step someBubbles tick 