module Main (main) where

import System.Environment (getArgs)
import System.IO (hFlush, stdout)

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

playDummy = do
  putStrLn "1"
  putStrLn "I"
  putStrLn "0"


playLoop = do
  playDummy
  hFlush stdout -- ! important std are buffered
  readOpponent
  playLoop


main = do
  [player_id] <- getArgs
  case player_id of
    "0" -> playLoop
    "1" -> do readOpponent
              playLoop
    _ -> fail $ "Invalid player id: " ++ player_id

