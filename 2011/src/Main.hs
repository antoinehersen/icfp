module Main (main) where

import System.Environment (getArgs)
import System.IO (hFlush, stdout, isEOF)

import Cards
import Actions


readOpponent = do
  application_side <- getLine
  case application_side of
    "1" -> do
      card_name <- getLine
      slot_ix <- getLine
      return (application_side, card_name, slot_ix)
    "2" -> do
      slot_ix <- getLine
      card_name <- getLine
      return (application_side, card_name, slot_ix)
    _ -> fail $ "Invalid application side " ++ application_side

playMovePutDec = do
  putStrLn "2"
  putStrLn "0"
  putStrLn "dec"

playMovePutZero = do
  putStrLn "2"
  putStrLn "0"
  putStrLn "zero"

playDummy = playMove idleMove


-- TODO add handling of end of file using isEOF
doTurn move = do
  playMove move
  hFlush stdout -- ! important std are buffered
  readOpponent

playLoop :: [Move] -> IO ()
playLoop moves = mapM_ doTurn (moves ++ (repeat idleMove))



main = do
  [player_id] <- getArgs
  let strategy  = repeat idleMove

  case player_id of
    "0" -> playLoop strategy
    "1" -> do readOpponent
              playLoop strategy
    _ -> fail $ "Invalid player id: " ++ player_id

