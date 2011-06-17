module CraterDirection where

import ParseMsg
import MapManagement ( inCircle )
import SteeringUtils
import Debug.Trace

{-
ASSUME TELEMETRY ONLY
maybe not the best idea in the long run
( but after all it is a pretty short run )
-}

-- take the best strategy
chooseStrategy :: Strategy -> [Strategy] -> Strategy
chooseStrategy defaultAct strgs = maximum ( defaultAct :strgs )

-- the function that should be used from outside
--- Init Telemetry
steering :: Message -> Message -> Steering
steering init@(Init _ _ _ _ _ maxspeed _ _) mesg@(Telemetry _ _ _ _ _ speed objs) = if and [density > 20, speed > maxspeed * 0.5]
	then Brake
	else if speed == 0
		then Accel
		else steer
    where
		density = length objs
		(Strategy steer _) = chooseStrategy defaultAct touchs
		defaultAct = homingSteer init mesg
		touchs = intersect (justAHead mesg) (msg2BouldersCirc mesg)

intersect :: [ (Vec2, Strategy) ] -> [ Circle ] -> [Strategy]
intersect points circles= trace (show toto) toto
    where toto = [ st | x@(xp,st) <- points , y <- circles , inCircle y xp]

-- Point just ahead of the rover
justAHead :: Message -> [( Vec2 ,Strategy )]
justAHead (Telemetry _ _ _ p@(px, py) dir speed _) =
    --[((tx, ty) , Strategy TurnRight 3 )]
	controlPoints dir speed p
    where (vdirx, vdiry) = angleToVec dir
          tx = 1.5 * vdirx * speed + px
          ty = 1.5 * vdiry * speed + py
          pts = controlPoints dir speed p


objToCircle :: Obj -> Circle
objToCircle (Obj _ vp r) = (vp, radiusMultiplier r)
objToCircle (Martian vp _ _ ) = (vp, 0.4)				-- en meme temps on ignore les martiens...

onlyBoulders :: [Obj] -> [Obj]
onlyBoulders = filter boulder
    where boulder (Obj Boulder _ _) = True
          boulder (Obj Crater _ _) = True
          boulder _ = False


msg2BouldersCirc :: Message -> [ Circle ]
msg2BouldersCirc (Telemetry _ _ _ _ _ _ objs) = map objToCircle $ onlyBoulders objs


-- Converges to (0, 0), regardless of obstacles reported by the Telemetry
-- InitMessage Telemetry
homingSteer :: Message -> Message -> Strategy
homingSteer init@(Init _ _ maxtime _ _ _ _ _) (Telemetry time _ _ vp dir _ objs) = Strategy steer priority
 where myVec = angleToVec dir
       steer = case whereIs myVec vp of
                 ToTheLeft -> RightAndAccel
                 ToTheRight -> LeftAndAccel
       priority = round (160 * 1 / ( maxtime - time + 1 ))