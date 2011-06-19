module Main (main) where

import System.Environment (getArgs)
import System.IO (hFlush, stdout, stdin, stderr, hIsClosed, hPutStrLn )
import Control.Monad

import Cards
import Actions
import Strategies
import Interpreter


readOpponent :: IO Move
readOpponent = do
  application_side <- (fmap readSide) getLine
  case application_side of
    LeftApp -> do
      card_name <- (fmap readCard) getLine
      slot_ix <- (fmap read) getLine
      return (Move application_side card_name slot_ix)
    RightApp -> do
      slot_ix <- (fmap read) getLine
      card_name <- (fmap readCard) getLine
      return (Move application_side card_name slot_ix)



playDummy :: IO ()
playDummy = playMove idleMove


pWorldIfDone world = do end <- hIsClosed stdin
                        when end $ hPutStrLn stderr (showWorld world)

doTurn world my_move = do
  playMove my_move
  hFlush stdout -- ! important std are buffered
  let new_world = updateProponent world my_move
  catch (do opp_move <- readOpponent
            let new_new_world = updateOpponent new_world opp_move
            return new_new_world)
        (\err -> do hPutStrLn stderr (show err)
                    --  hPutStrLn stderr (showWorld new_world)
                    return new_world )

playLoop :: World -> [Move] -> IO ()
playLoop world moves = foldM_ doTurn world (take 100000 (moves ++ (repeat idleMove)))

playSoloLoop = do playMoves $ optimalArg 0 killPts 12
            --      playMoves $ healMax 10000 1 2
              --    playMoves $ attack killPts 1 2 (256 - 33 )
                  hFlush stdout
                  interact id
-- playMoves soloStrategy
--                   playMoves $ maxNb 3
--                   playMoves $ nbToMoves 27 31
--                   playMoves ( [ Move RightApp Get 9 ] ++ applyNbMoves 31 9 )
--                   playMoves $ nbToMoves 7 5
--                   playMoves $ nbToMoves 5 8
--                   playMoves ( [ Move RightApp Get 11 ] ++ getArgFromMoves 8 11 )
--                   playMoves $ clean 1
--                   playMoves $ heal 1098 1 2
--                   playMoves $ clean 0
--                   playMoves $ setNb 31 0
--                   playMoves $ addFrom 87 31 0
--                   playMoves $ healMax 10000 1 2
--                  playMoves $ attack 10000 1 2


main = do
  [player_id] <- getArgs
  let strategy = snipeTarg [0..7] ++ maxAttack 1 [ 8 .. 255 ] ++ strategySimpleAttack
  let world = defaultWorld

  case player_id of
    "0" -> playLoop world strategy
    "1" -> do opp_move <- readOpponent
              playLoop (updateOpponent world opp_move) strategy
    "3" -> playSoloLoop
    _ -> fail $ "Invalid player id: " ++ player_id

