module Main (main) where

import System.Environment (getArgs)
import System.IO (hFlush, stdout, stdin, stderr, hIsClosed, hPutStrLn )
import System.IO (BufferMode (..), hSetBuffering )

import Control.Monad

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan

import Cards
import Actions
import Strategies
import Interpreter

import Viz (initGL)


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

doTurn worldM world my_move = do
  playMove my_move
  hFlush stdout -- ! important std are buffered
  let new_world = updateProponent world my_move
  writeChan  worldM new_world
  catch (do opp_move <- readOpponent
            let ! new_new_world = updateOpponent new_world opp_move
            return new_new_world)
        (\err -> do hPutStrLn stderr (show err)
                    hPutStrLn stderr (showWorld new_world)
                    return new_world )


playLoop worldM world moves = do
  final <- foldM (doTurn worldM ) world (take 100000 (moves ++ (repeat idleMove)))
  hPutStrLn stderr (showWorld final)


playSoloLoop = do playMoves $ attack 10000 12 33 0
                  playMoves $ reviveStr 12 0
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
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering

  [player_id] <- getArgs
  let strategy =  finalStrategy ++ infYinYanWave
  let world = defaultWorld

  worldM <- newChan
  writeChan worldM world
  forkIO $ initGL worldM

  case player_id of
    "0" -> playLoop worldM world strategy
    "1" -> do opp_move <- readOpponent
              playLoop worldM (updateOpponent world opp_move) strategy
    "3" -> playSoloLoop
    _ -> fail $ "Invalid player id: " ++ player_id

