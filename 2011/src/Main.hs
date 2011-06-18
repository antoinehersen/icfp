module Main (main) where

import System.Environment (getArgs)
import System.IO (hFlush, stdout)
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




-- TODO add handling of end of file using isEOF
doTurn world my_move = do
  playMove my_move
  hFlush stdout -- ! important std are buffered
  opp_move <- readOpponent
  let new_word = updateOpponent ( updateProponent world my_move ) opp_move
  return new_word

playLoop :: World -> [Move] -> IO ()
playLoop world moves = foldM_ doTurn world (moves ++ (repeat idleMove))



main = do
  [player_id] <- getArgs
  let strategy = strategySimpleAttack
  let world = defaultWorld

  case player_id of
    "0" -> playLoop world strategy
    "1" -> do opp_move <- readOpponent
              playLoop (updateOpponent world opp_move) strategy
    _ -> fail $ "Invalid player id: " ++ player_id

