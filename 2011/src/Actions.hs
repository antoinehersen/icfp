module Actions where

import Cards

data ApplicationSide = LeftApp | RightApp

instance Show ApplicationSide where
    show LeftApp  = "1"
    show RightApp = "2"

data Move = Move ApplicationSide Card Int deriving (Show)

playMove :: Move -> IO ()
playMove (Move side card idx) = case side  of
                                  LeftApp -> do putStrLn (show side)
                                                putStrLn (show card)
                                                putStrLn (show idx )
                                  RightApp -> do putStrLn (show side)
                                                 putStrLn (show idx )
                                                 putStrLn (show card)


--- Examples of move
idleMove :: Move
idleMove = Move LeftApp I 0