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

pseudoMartianSize = 5


-- take the best strategy
chooseStrtgHighest :: Strategy -> [Strategy] -> Strategy
chooseStrtgHighest defaultAct strtgs = maximum (trace (show all) all)
    where all = defaultAct : strtgs --(chooseStrtgAggregte strtgs )

-- Not as helpfull as I wish
chooseStrtgAggregte :: [Strategy] -> [ Strategy ] -- should have used maybe
chooseStrtgAggregte strtgs = win
    where leftSum  = sum [ i | Strategy TurnLeft  i <- strtgs ]
          rightSum = sum [ i | Strategy TurnRight i <- strtgs ]
          win | (leftSum <= rightSum) && (rightSum /= 0)  = [Strategy TurnRight  rightSum]
              | leftSum > rightSum = [Strategy TurnLeft leftSum]
              | otherwise = []

--madAllien :: Message -> Strategy


-- the function that should be used from outside
--- Init Telemetry
steering :: Message -> Message -> Steering
steering init@(Init _ _ _ _ _ maxspeed _ _) mesg@(Telemetry _ _ _ _ _ speed objs)
    | and [density > 15, speed > maxspeed * 0.45] = steerBrake steer
    | speed > (maxspeed * 0.8 ) = steerBrake steer
    | speed < (maxspeed * 0.05) = steerAcc steer -- keep the show moving
    | otherwise = steer
    where
      density = length objs
      (Strategy steer _) = chooseStrtgHighest defaultAct touchs
      defaultAct = homingSteer init mesg
      touchs = intersect (justAHead mesg) (msg2Circs mesg)

intersect :: [ (Vec2, Strategy) ] -> [ ( ObjType ,Circle) ] -> [Strategy]
intersect points circles= toto -- trace (show toto) toto
    where toto = [ Strategy t (i* (scareFactor obj))  | x@(xp, Strategy t i ) <- points , (obj,y) <- circles , inCircle y xp]

-- Change strategy importance in fonction of type of object
scareFactor :: ObjType -> Int
scareFactor EvilMartian = 2
scareFactor Boulder = 1
scareFactor Crater = 3

-- Point just ahead of the rover
justAHead :: Message -> [( Vec2 ,Strategy )]
justAHead (Telemetry _ _ _ p@(px, py) dir speed _) =
    --[((tx, ty) , Strategy TurnRight 3 )]
	controlPoints dir speed p
justAHead _ = []


objToCircle :: Obj -> ( ObjType , Circle)
objToCircle (Obj objType vp r) = ( objType , (vp, r))
objToCircle (Martian vp _ _ ) = (EvilMartian , (vp, pseudoMartianSize) )-- bigger than they really are

sizeChange :: [Obj] -> [Obj]
sizeChange = map coef
    where coef (Obj Crater p r) = Obj Crater p (r* 1.3)
          coef x = x

homeFilter :: [Obj] -> [Obj]
homeFilter= filter homeF
    where homeF (Obj Home _ _) = False
          homeF _ = True


msg2Circs :: Message -> [ (ObjType, Circle) ]
msg2Circs (Telemetry _ _ _ _ _ _ objs) = map objToCircle (sizeChange (homeFilter objs))


-- Converges to (0, 0), regardless of obstacles reported by the Telemetry
-- InitMessage Telemetry
homingSteer :: Message -> Message -> Strategy
homingSteer init@(Init _ _ maxtime _ _ _ _ _) (Telemetry time _ turn vp dir _ objs) = Strategy steer priority
 where myVec = angleToVec dir
       steer = case whereIs myVec vp of
                 ToTheLeft -> smoothTurn RightAndAccel dir turn vp
                 ToTheRight -> smoothTurn LeftAndAccel dir turn vp
       priority = round (1 + 200 * ( time / maxtime ) ** 4)


smoothTurn :: Steering -> Double -> Turn -> Vec2 -> Steering
smoothTurn go dir turn p@(px, py) =
	if angle > 3
		then goStraight turn
		else go
	where
		(vx, vy) = angleToVec dir
		norm = sqrt (px*px + py*py)
		vectprod = (vx*px + vy*py) / norm
		angle = acos vectprod

goStraight turn = case turn of
	HardLeft -> RightAndAccel
 	NormalLeft -> RightAndAccel
	HardRight -> LeftAndAccel
	NormalRight -> LeftAndAccel
	Straight -> Accel