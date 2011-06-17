module SteeringUtils where
-- import Debug.Trace

data WhereIs = ToTheLeft
	     | ToTheRight

type Vec2 = (Double, Double)

data Steering = Accel
	      | Brake
	      | TurnLeft
	      | TurnRight
	      | LeftAndAccel
	      | LeftAndBrake
	      | RightAndAccel
	      | RightAndBrake
	      | DoNothing
                deriving Eq

type Circle = ( Vec2, Double)


-- Choose the one with max Int
data Strategy = Strategy Steering Int deriving (Eq, Show)

-- make them Sortable
instance Ord Strategy where
    compare ( Strategy _ a) (Strategy _ b) = compare a b

steerAcc = steerAcc'

steerAcc' Accel = Accel
steerAcc' Brake = DoNothing
steerAcc' TurnLeft = LeftAndAccel
steerAcc' TurnRight = RightAndAccel
steerAcc' LeftAndAccel = LeftAndAccel
steerAcc' LeftAndBrake = TurnLeft
steerAcc' RightAndAccel = RightAndAccel
steerAcc' RightAndBrake = TurnRight
steerAcc' DoNothing = Accel


steerBrake  = steerBrake'

steerBrake' Accel = DoNothing
steerBrake' Brake = Brake
steerBrake' TurnLeft = LeftAndBrake
steerBrake' TurnRight = RightAndBrake
steerBrake' LeftAndAccel = TurnLeft
steerBrake' LeftAndBrake = LeftAndBrake
steerBrake' RightAndAccel = TurnRight
steerBrake' RightAndBrake = RightAndBrake
steerBrake' DoNothing = Brake



-- X product is the determinant
-- axb = |a||b| sin theta
crossProduct :: Vec2 -> Vec2 -> Double
crossProduct (ax,ay) (bx,by) = ax*by - bx*by

-- a.b = |a||b| cos theta
dotProduct  :: Vec2 -> Vec2 -> Double
dotProduct  (ax,ay) (bx,by) = ax*bx + ay*by

-- lenghVec a b = sqrt  crossProduct a b

unitVec = (1, 0) :: Vec2

vecRotation (x, y) theta = (rx, ry)
    where rx = ( x * (cos theta)) - (y * (sin theta))
          ry = (  x * (sin theta)) + (y * (cos theta))

scalVec (x,y) a = ( a*x, a*y)

-- Returns a vector of norm 1
angleToVec :: Double -> Vec2
angleToVec a = (cos $ toRadian a, sin $ toRadian a)

addVec2 (ax, ay) (bx, by) = (ax + bx , ay + by)

-- Converts degrees to radians
toRadian :: Double -> Double
toRadian a = a * pi / 180

maxAngle = 32
maxDist = 1.6

-- initial control points
initCtrlPts :: [ Vec2 ]
initCtrlPts = [ scalVec vec r | vec <- vecs , r <- dists ]
    where angles = map toRadian [0, 1, 2, 4, 8, 16, maxAngle ] -- list of angles
          allAngles = angles ++ ( map negate angles ) -- take angles in both directon
          vecs =  map (vecRotation unitVec) allAngles -- make vector out of them
          dists = [ 0.1 , 0.2 , 0.4 , 0.8, maxDist ]


assignStrategy2 :: Vec2 -> (Vec2, Strategy)
assignStrategy2 a@(x,y) = (a, Strategy dir int )
    where int = round $ 500 * ( 0.05 + (maxDist - x ) ** 2 )
          dir | y < 0 = TurnLeft
              | otherwise = TurnRight

initPntAndStrats = map assignStrategy2 initCtrlPts


-- rover angle -> speed -> rover position
controlPoints :: Double -> Double -> Vec2 -> [(Vec2, Strategy)]
controlPoints angle speed p = [ (transform vec, strg) | (vec, strg) <- initPntAndStrats ]
    where transform =  (addVec2 p) . ( (flip scalVec speed ) .( flip vecRotation (toRadian angle )))



dist :: Vec2 -> Vec2 -> Double
dist a@(ax, ay) b@(bx, by) = sqrt (dx*dx + dy*dy)
	where dx = ax - bx
	      dy = ay - by

-- The vector is assumed to be of norm 1
pointFromVector :: (Double, Double) -> Double -> Vec2 -> (Double, Double)
pointFromVector (ox, oy) fact ( dx, dy) = (ox + fact * dx, oy + fact * dy)

-- Tells is the second vector is orientated to the left or the right of the first one
whereIs :: Vec2 -> Vec2 -> WhereIs
whereIs ( a1, a2) ( b1, b2) | crossProdZ < 0 = ToTheRight
                            | otherwise = ToTheLeft
    where crossProdZ = (a1 * b2) - (a2 * b1)

-- multiplies the radius of a circle by a number inversely proportional to it
radiusMultiplier :: Double -> Double
radiusMultiplier r =
	if r < 20
		then r * 2
		else r * 1.2

-- To send directions to tcontroller
instance Show Steering where
	show = showSteering

showSteering :: Steering -> String
showSteering Accel = "a;"
showSteering Brake = "b;"
showSteering TurnLeft = "l;"
showSteering TurnRight = "r;"
showSteering LeftAndAccel = "al;"
showSteering LeftAndBrake = "bl;"
showSteering RightAndAccel = "ar;"
showSteering RightAndBrake = "br;"
showSteering DoNothing = ";"