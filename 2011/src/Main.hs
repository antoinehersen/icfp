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

playSoloLoop = do mapM_ playMove soloStrategy
                  mapM_ playMove $ maxNb 3
                  mapM_ playMove $ nbToMoves 27 31
                  mapM_ playMove ( [ Move RightApp Get 9 ] ++ applyNbMoves 31 9 )
                  hFlush stdout
                  interact id

main = do
  [player_id] <- getArgs
  let strategy = strategySimpleAttack
  let world = defaultWorld

  case player_id of
    "0" -> playLoop world strategy
    "1" -> do opp_move <- readOpponent
              playLoop (updateOpponent world opp_move) strategy
    "3" -> playSoloLoop
    _ -> fail $ "Invalid player id: " ++ player_id

