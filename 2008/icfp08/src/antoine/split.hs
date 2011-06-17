module Main where

import Debug.Trace

test = "123213 321321 3213 "

splitBy p s = case dropWhile p s of
                [] -> []
                s' -> w : splitBy p s''
                      where (w, s'') = break p s'


main = do
  putStrLn "Hello"
  putStrLn $ show ( splitBy (== ' ') test)

