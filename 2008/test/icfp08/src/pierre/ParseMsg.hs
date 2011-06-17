module ParseMsg  where

    -- Naive data type representing a message
    -- To be refined / wrapped later

-- T time-stamp vehicle-ctl vehicle-x vehicle-y vehicle-dir vehicle-speed objects ;

data Message = Init !Double !Double !Double !Double !Double !Double !Double !Double
	     | Telemetry !Double VState Turn !(Double, Double) !Double !Double [Obj]
	     | BoulderCrash !Double
	     | CraterFall !Double
	     | KilledByMartian !Double
	     | EndOfRun Double !Double
	     | Success !Double
	       deriving (Show, Eq)


data VState = Accelerating
	    | Braking
	    | Rolling
	      deriving (Show, Eq)

data Turn = NormalLeft
	  | HardLeft
	  | NormalRight
	  | HardRight
	  | Straight
	    deriving (Show, Eq)

data ObjType = Boulder | Crater | Home deriving (Show, Eq)

data Obj = Obj !ObjType !(Double, Double) !Double
         | Martian !(Double, Double) !Double !Double deriving (Show, Eq)


parse :: String -> Message
parse s	= let splitStr = words s in
	  case splitStr !! 0 of
	    "I" -> parseInit splitStr
	    "T" -> parseTele splitStr
	    "B" -> BoulderCrash (readDouble $ splitStr !! 1)
	    "C" -> CraterFall (readDouble $ splitStr !! 1)
	    "K" -> KilledByMartian (readDouble $ splitStr !! 1)
	    "E" -> EndOfRun (readDouble $ splitStr !! 1) (readDouble $ splitStr !! 2)
	    "S" -> Success (readDouble $ splitStr !! 1)

-- Parses an Initialization message
parseInit :: [String] -> Message
parseInit array	= let vals = map readDouble array in
		  Init (vals !! 1) (vals !! 2) (vals !! 3) (vals !! 4) (vals !! 5) (vals !! 6) (vals !! 7) (vals !! 8)

-- Parses a Telemetry message
parseTele :: [String] -> Message
parseTele array	= let (tele, objs) = splitAt 7 array in
		  Telemetry
		  (readDouble $ tele !! 1)
		  (readAccel $ tele !! 2)
		  (readTurn $ tele !! 2)
		  ( (readDouble $ tele !! 3) , (readDouble $ tele !! 4) )
		  (readDouble $ tele !! 5)
		  (readDouble $ tele !! 6)
		  (readObjects objs)

-- The martian takes up one more value
readObjects :: [String] -> [Obj]
readObjects [] = []
readObjects objs@("m":_) = let (o, os) = splitAt 5 objs in
			   readObj o : readObjects os
readObjects objs = let (o, os) = splitAt 4 objs in
		   readObj o : readObjects os

-- Read individual tokens
readDouble :: String -> Double
readDouble s = read s

readAccel :: String -> VState
readAccel s = case s !! 0 of
		'a' -> Accelerating
		'b' -> Braking
		'-' -> Rolling

readTurn :: String -> Turn
readTurn s = case s !! 1 of
	       'l' -> NormalLeft
	       'L' -> HardLeft
	       'r' -> NormalRight
	       'R' -> HardRight
	       '-' -> Straight

readObj :: [String] -> Obj
readObj [a, b, c, d] = Obj objType (readDouble b , readDouble c) (readDouble d)
    where objType = case a of
                      "b" -> Boulder
		      "c" -> Crater
		      "h" -> Home
readObj [a, b, c, d, e]	= Martian (readDouble b ,readDouble c) (readDouble d) (readDouble e)
