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

pseudoMartianSize = 6


-- take the best strategy
chooseStrategy :: Strategy -> [Strategy] -> Strategy
chooseStrategy defaultAct strgs = maximum (trace (show all) all)
    where all = defaultAct :strgs


-- the function that should be used from outside
--- Init Telemetry
steering :: Message -> Message -> Steering
steering init@(Init _ _ _ _ _ maxspeed _ _) mesg@(Telemetry _ _ _ _ _ speed objs)
    | and [density > 20, speed > maxspeed * 0.5] = steerBrake steer
    | speed > (maxspeed * 0.8 ) = steerBrake steer
    | speed < (maxspeed * 0.05) = steerAcc steer
    | otherwise = steer
    where
      density = length objs
      (Strategy steer _) = chooseStrategy defaultAct touchs
      defaultAct = homingSteer init mesg
      touchs = intersect (justAHead mesg) (msg2Circs mesg)

intersect :: [ (Vec2, Strategy) ] -> [ Circle ] -> [Strategy]
intersect points circles= toto -- trace (show toto) toto
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
justAHead _ = []


objToCircle :: Obj -> Circle
objToCircle (Obj _ vp r) = (vp, r)
objToCircle (Martian vp _ _ ) = (vp, pseudoMartianSize) -- bigger than they really are

sizeChange :: [Obj] -> [Obj]
sizeChange = map coef
    where coef (Obj Crater p r) = Obj Crater p (r* 1.8)
          coef x = x

homeFilter :: [Obj] -> [Obj]
homeFilter= filter homeF
    where homeF (Obj Home _ _) = False
          homeF _ = True


msg2Circs :: Message -> [ Circle ]
msg2Circs (Telemetry _ _ _ _ _ _ objs) = map objToCircle (sizeChange (homeFilter objs))


-- Converges to (0, 0), regardless of obstacles reported by the Telemetry
-- InitMessage Telemetry
homingSteer :: Message -> Message -> Strategy
homingSteer init@(Init _ _ maxtime _ _ _ _ _) (Telemetry time _ turn vp dir _ objs) = Strategy steer priority
 where myVec = angleToVec dir
       steer = case whereIs myVec vp of
                 ToTheLeft -> smoothTurn RightAndAccel dir turn vp
                 ToTheRight -> smoothTurn LeftAndAccel dir turn vp
       priority = round (200 * ( time / maxtime ) ** 4)


smoothTurn :: Steering -> Double -> Turn -> Vec2 -> Steering
smoothTurn go dir turn p@(px, py) =
	if angle > 3
		then goStraight turn
		else go
	where
		(vx, vy) = angleToVec dir
		norm = sqrt (px*px + py*py)
		vectprod = (vx*px + vy*py) / norm
		angle = trace (show $ acos vectprod) (acos vectprod)

goStraight turn = case turn of
	HardLeft -> RightAndAccel
 	NormalLeft -> RightAndAccel
	HardRight -> LeftAndAccel
	NormalRight -> LeftAndAccel
	Straight -> Accel