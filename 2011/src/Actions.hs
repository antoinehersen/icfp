module Actions where

import Cards

data ApplicationSide = LeftApp | RightApp

instance Show ApplicationSide where
    show LeftApp  = "1"
    show RightApp = "2"

readSide :: String -> ApplicationSide
readSide "1" = LeftApp
readSide "2" = RightApp
readSide s = error $ "Not a side: " ++ s

data Move = Move ApplicationSide Card Int deriving (Show)

playMove :: Move -> IO ()
playMove (Move side card idx) = case side  of
                                  LeftApp -> do putStrLn (show side)
                                                putStrLn (show card)
                                                putStrLn (show (idx `mod` 255))
                                  RightApp -> do putStrLn (show side)
                                                 putStrLn (show (idx `mod` 255))
                                                 putStrLn (show card)


--- Examples of move
idleMove :: Move
idleMove = Move LeftApp I 0