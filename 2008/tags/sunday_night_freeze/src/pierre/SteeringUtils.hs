module SteeringUtils where
import Debug.Trace

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

-- Returns a vector of norm 1
angleToVec :: Double -> Vec2
angleToVec a = (cos $ toRadian a, sin $ toRadian a)


-- Converts degrees to radians
toRadian :: Double -> Double
toRadian a = a * pi / 180

-- rover angle -> speed -> rover position
controlPoints :: Double -> Double -> Vec2 -> [(Vec2, Strategy)]
controlPoints angle speed p =
	map (assignStrategy p (angleToVec angle)) ptList
	where
		ptList = (map (pointFromVector p factor) vecs) ++ (map (pointFromVector p factor') vecs)
		base = [-15, 0, 15]
		angles = map (\x -> angle + x) base
		vecs = map angleToVec angles
		factor = speed * 0.5
		factor' = speed * 1

-- associate a strategy to a point
-- rover position -> rover direction -> control point -> Strategy
assignStrategy :: Vec2 -> Vec2 -> Vec2 -> (Vec2, Strategy)
assignStrategy o@(ox, oy) d p@(px, py) = (p, Strategy steer priority)
	where
		priority = round (150 / (dist o p))
		steer = case whereIs d (-ox, -oy) of
			ToTheRight -> if priority > 30
				then LeftAndBrake
				else TurnLeft
			ToTheLeft -> if priority > 30
				then RightAndBrake
				else TurnRight

dist :: Vec2 -> Vec2 -> Double
dist a@(ax, ay) b@(bx, by) = sqrt (dx*dx + dy*dy)
	where
		dx = ax - bx
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

-- To send directions to the controller
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