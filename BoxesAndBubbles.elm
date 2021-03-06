module BoxesAndBubbles where
-- based roughly on http://gamedevelopment.tutsplus.com/tutorials/gamedev-6331

-- plain old pair for coordinates, vectors
type Vec2 = (Float,Float)

type Body_ b = { b |
  velocity: Vec2, -- direction and speed
  inverseMass: Float, -- we usually use only inverse mass for calculations
  restitution: Float -- bounciness factor
}
-- it's all about the bubble
type Bubble_ = Body_ { 
  radius: Float, pos: Vec2 -- center position
}

type Box_ = Body_ {  
  min: Vec2, max: Vec2
}

data Body = Box Box_ | Bubble Bubble_

-- basic bubble with some defaults
makeBubble radius pos velocity = 
  makeBubble2 radius pos velocity 1 1

makeBubble2 radius pos velocity density restitution = 
  Bubble { radius = radius, pos = pos, velocity = velocity, 
  inverseMass = 1/(pi*radius*radius*density), 
  restitution = restitution }

-- just vector things

plus: Vec2 -> Vec2 -> Vec2
plus (x0,y0) (x1,y1) = (x0+x1,y0+y1)

minus: Vec2 -> Vec2 -> Vec2
minus (x0,y0) (x1,y1) = (x0-x1,y0-y1)

dot: Vec2 -> Vec2 -> Float
dot (x0,y0) (x1,y1) = x0*x1 + y0*y1

div2: Vec2 -> Float -> Vec2
div2 (x,y) a = (x/a, y/a)

mul2: Vec2 -> Float -> Vec2
mul2 (x,y) a = (x*a, y*a)

-- squared norm/length of ector
lenSq: Vec2 -> Float
lenSq (x,y) = x*x + y*y


-- collision calculation for different types of bodies

type CollisionResult = { normal: Vec2, penetration: Float }

-- calculate collision normal, penetration depth of a collision among bubbles
-- simple optimization: doesn't compute sqrt unless necessary
collisionBubbleBubble: Bubble -> Bubble -> CollisionResult
collision b0 b1 = 
  let
    b0b1 = minus b1.pos b0.pos
    radiusb0b1 = b0.radius+b1.radius
    distanceSq = lenSq b0b1
  in
    if | distanceSq == 0 -> { normal = (1,0), penetration = b0.radius } -- same position, arbitrary normal
       | distanceSq >= radiusb0b1*radiusb0b1 -> { normal = (1,0), penetration = 0 } -- no intersection, arbitrary normal
       | otherwise -> 
          let d = sqrt distanceSq
          in { normal = div2 b0b1 d, penetration = radiusb0b1 - d }

collisionBoxBox: Box -> Box -> CollisionResult
collision b0 b1 = {normal = (0,0), penetration = 0}

collisionBoxBubble: Box -> Bubble -> CollisionResult
collision box bubble = {normal = (0,0), penetration = 0}

collision: Body -> Body -> CollisionResult
collision body0 body1 = case (b0,b1) of
  (Bubble b0, Bubble b1) -> collisionBubbleBubble b0 b1
  (Box b0, Box b1) -> collisionBoxBox b0 b1
  (Box box, Bubble bubble) -> collisionBoxBubble box bubble
  (Bubble bubble, Box box) -> collisionBoxBubble box bubble


-- modify bodies' trajectories when they collide
resolveCollision: CollisionResult -> Body a -> Body a -> (Body a, Body a)
resolveCollision {normal,penetration} b0 b1 = 
  let 
    relativeVelocity = minus b1.velocity b0.velocity
    velocityAlongNormal = dot relativeVelocity normal
  in 
    if penetration == 0 || velocityAlongNormal > 0 then (b0,b1) -- no collision or velocities separating
    else let
      restitution = min b0.restitution b1.restitution -- collision restitution
      invMassSum = (b0.inverseMass + b1.inverseMass)
      j = (-(1 + restitution) * velocityAlongNormal) / invMassSum -- impulse scalar
      impulse = mul2 normal j
    in ({ b0 | velocity <- minus b0.velocity (mul2 impulse b0.inverseMass) },
        { b1 | velocity <- plus b1.velocity (mul2 impulse b1.inverseMass) })


-- collide a0 with all the bubbles, modifying b along the way.
-- return (updated a0, [updated bubbles])
collideWith: Body a -> [Body a] -> [Body a] -> [Body a]
collideWith a0 bubbles acc = case bubbles of
  [] -> a0 :: acc
  (b0 :: bs) -> 
    let collisionResult = collision a0 b0
        (a1,b1) = resolveCollision collisionResult a0 b0
    in collideWith a1 bs (b1 :: acc)

-- recursive collision resolution
collide: [Bubble] -> [Bubble] -> [Bubble]
collide acc bubbles = 
  case bubbles of
    [] -> acc
    h::t -> 
      let (h1 :: t1) = collideWith h t []
      in collide (h1::acc) t1


-- applies accellerating force, does movement and resolves collisions for all the bubbles
step: Vec2 -> [Bubble] -> [Bubble]      
step force bubbles = 
  let update bubble = { bubble | pos <- plus bubble.pos bubble.velocity }
  in map update (collide [] bubbles)
  -- resolve all collisions; optimization: broad phase
  -- TODO apply forces


