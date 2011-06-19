module Strategies where

import Cards
import Actions

strategyDoNothing = repeat idleMove

strategySimpleAttack = cycle [ Move RightApp Dec 0, Move RightApp Zero 0 ]

soloStrategy = [ Move RightApp Zero 0 , Move LeftApp Succ 0 ] ++ (replicate 16 (Move LeftApp Dbl 0 ))

maxNb i  = [ Move RightApp Zero i , Move LeftApp Succ i ] ++ (replicate 16 (Move LeftApp Dbl i ))

nbToCards :: Int -> [Card]
nbToCards = reverse . nbToCards'
    where
      nbToCards' 0 = [Zero]
      nbToCards' i | (i `mod` 2) == 0 = Dbl : (nbToCards' (i `quot` 2 ))
                   | otherwise = Succ : ( nbToCards' $ i - 1 )

nbToMoves nb i = map tr $ nbToCards nb
    where
      tr Zero = Move RightApp Zero i
      tr a = Move LeftApp a i

applyNbCards n = let nb_cards = reverse $ nbToCards n
                 in add_ks nb_cards
    where
      add_ks [Zero] = [Zero]
      add_ks (x:xs) = [K, S , x ] ++ add_ks xs

applyNbMoves nb i = map tr $ applyNbCards nb
    where
--      tr Zero = Move RightApp Zero i
      tr K = Move LeftApp K i
      tr S = Move LeftApp S i
      tr x = Move RightApp x i




-- *Strategies> map (length .nbToCards)  [0 .. 255]
-- [1,2,3,4,4,5,5,6,5,6,6,7,6,7,7,8,6,7,7,8,7,8,8,9,7,8,8,9,8,9,9,10,7,8,8,9,8,9,9,10,8,9,9,10,9,10,10,11,8,9,9,10,9,10,10,11,9,10,10,11,10,11,11,12,8,9,9,10,9,10,10,11,9,10,10,11,10,11,11,12,9,10,10,11,10,11,11,12,10,11,11,12,11,12,12,13,9,10,10,11,10,11,11,12,10,11,11,12,11,12,12,13,10,11,11,12,11,12,12,13,11,12,12,13,12,13,13,14,9,10,10,11,10,11,11,12,10,11,11,12,11,12,12,13,10,11,11,12,11,12,12,13,11,12,12,13,12,13,13,14,10,11,11,12,11,12,12,13,11,12,12,13,12,13,13,14,11,12,12,13,12,13,13,14,12,13,13,14,13,14,14,15,10,11,11,12,11,12,12,13,11,12,12,13,12,13,13,14,11,12,12,13,12,13,13,14,12,13,13,14,13,14,14,15,11,12,12,13,12,13,13,14,12,13,13,14,13,14,14,15,12,13,13,14,13,14,14,15,13,14,14,15,14,15,15,16]