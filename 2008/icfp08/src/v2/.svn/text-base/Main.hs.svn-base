module Main where

import ParseMsg
--import BasicDirections
import CraterDirection
import SteeringUtils
-- import Viz (goViz)

--import Data.Bits
import Network.Socket
-- import Network.BSD (getProtocolNumber)
import Data.List
import System.IO

import Control.Concurrent

import System.Environment (getArgs)

{-
Speed of the server [2.14kB/s] !!!
-}


main = do
  [hostname, portStr] <- getArgs
--   protNb <- getProtocolNumber "tcp"
  let protNb = 6
  let hints = defaultHints { addrFlags = [AI_ADDRCONFIG],
                             addrFamily = AF_INET,
                             addrProtocol = protNb }
  addrs <- getAddrInfo (Just hints) (Just hostname) (Just portStr)
  if (1 /= (length addrs))
    then putStrLn "Too many IP addres possible"
    else do
      let addr = head addrs
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock NoDelay 1
      connect sock (addrAddress addr)
      netHandle <- socketToHandle sock ReadWriteMode
      --hSetBuffering netHandle NoBuffering -- Maybe not a good idea
      streamEx netHandle


splitBy p s = case dropWhile p s of
                [] -> []
                s' -> w : splitBy p s''
                      where (w, s'') = break p s'

streamEx :: Handle -> IO ()
streamEx hdl = do
  str <- hGetContents hdl

  -- the main thread will spend the rest of her time feeding the stream
  let str' = splitBy (== ';') str
  let telemetryList = map parse str'
  let init = head telemetryList

  telemetryChan <- newChan
--   vizChan <- dupChan telemetryChan

  forkIO $ steeringThread init telemetryChan hdl -- handle to reply
--   forkIO $ goViz init vizChan

  writeList2Chan telemetryChan telemetryList



steeringThread init teleChan hdl = do
  telemetryList <- getChanContents teleChan
  let actionList = map (decide init) telemetryList
  mapM_ (\x -> (hPutStr hdl (show x)) >> (hFlush hdl)) actionList
  --mapM_ putStrLn  (map show telemetryList )
  --putStrLn $ show str'
  --putStrLn "<----------------->"

-- Init -> Tel -> steereg
decide :: Message -> Message -> Steering
decide init tele@(Telemetry _ _ _ _ _ _ _) = steering init tele
decide _ _ = DoNothing


-- streamEx :: Handle -> IO ()
-- streamEx hdl = do
--   c <- hGetChar hdl
--   putChar c
-- --  hFlush stdout
-- --  threadDelay 1
--   streamEx hdl