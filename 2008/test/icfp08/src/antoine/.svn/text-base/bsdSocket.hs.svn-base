module Main where

import ParseMsg

--import Data.Bits
import Network.Socket
import Network.BSD (getProtocolNumber)
import Data.List
import System.IO
import Control.Concurrent

import System.Environment (getArgs)

{-
Speed of the server [2.14kB/s] !!!
-}

main = do
  [hostname, portStr] <- getArgs
  protNb <- getProtocolNumber "tcp"
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

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p s = case dropWhile p s of
                [] -> []
                s' -> w : splitBy p s''
                      where (w, s'') = break p s'


streamEx :: Handle -> IO ()
streamEx hdl = do
  str <- hGetContents hdl
  let str' = splitBy (== ';') str
  let telemetryList = map parse str'
  mapM_ putStrLn  (map show telemetryList )

-- Streamex :: Handle -> IO ()
-- streamEx hdl = do
--   c <- hGetChar hdl
--   putChar c
-- --  hFlush stdout
-- --  threadDelay 1
--   streamEx hdl