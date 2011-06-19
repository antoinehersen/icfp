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

--- target_nb actuall_nb
addFromCards :: Int -> Int -> [Card]
addFromCards t a = reverse $ nbToCards' t a
    where
      nbToCards' t a | t <= a = []
                     | (t `mod` 2) == 0 && (t `quot` 2 ) >= a = Dbl : (nbToCards' (t `quot` 2 ) a )
                     | otherwise = Succ : ( nbToCards' (t - 1)  a )

addFromMoves t a i = map (\x -> Move LeftApp x i)  $ addFromCards t a

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


getArgFromMoves from i = [ Move LeftApp K i, Move LeftApp S i , Move RightApp Get i ] ++ ( applyNbMoves from i)

------------------------------------------------
------------------------------------------------
------------------------------------------------

setNb = nbToMoves

applyNb nb = applyNbMoves (nb `mod` 256)
getArgFrom from  = getArgFromMoves (from `mod` 256)
argByRef register nb i = clean register ++ setNb nb register ++ getArgFrom register i

optimalArg register nb i = let byVal = applyNb nb i
                               byArg = argByRef register nb i
                           in if (length byVal) < (length byArg)
                              then byVal
                              else byArg



addFrom = addFromMoves -- a bit retardet in retrospect

-- usr target for max arg too

clean i = [Move LeftApp Put i ]

heal pts target i = clean i
                    ++ [ Move RightApp Help i ] ++ optimalArg target target i ++ optimalArg target target i ++ optimalArg target pts i

healTo to from target i = let seq = healthSeq from to
                          in concatMap (\pts -> heal pts target i ) seq

healMax = healTo 65535


attack pts base target i = clean i
                           ++ [ Move RightApp Attack i ] ++ optimalArg base  base i ++ optimalArg base target i ++ optimalArg base pts i

-- *Strategies> map (length .nbToCards)  [0 .. 255]
-- [1,2,3,4,4,5,5,6,5,6,6,7,6,7,7,8,6,7,7,8,7,8,8,9,7,8,8,9,8,9,9,10,7,8,8,9,8,9,9,10,8,9,9,10,9,10,10,11,8,9,9,10,9,10,10,11,9,10,10,11,10,11,11,12,8,9,9,10,9,10,10,11,9,10,10,11,10,11,11,12,9,10,10,11,10,11,11,12,10,11,11,12,11,12,12,13,9,10,10,11,10,11,11,12,10,11,11,12,11,12,12,13,10,11,11,12,11,12,12,13,11,12,12,13,12,13,13,14,9,10,10,11,10,11,11,12,10,11,11,12,11,12,12,13,10,11,11,12,11,12,12,13,11,12,12,13,12,13,13,14,10,11,11,12,11,12,12,13,11,12,12,13,12,13,13,14,11,12,12,13,12,13,13,14,12,13,13,14,13,14,14,15,10,11,11,12,11,12,12,13,11,12,12,13,12,13,13,14,11,12,12,13,12,13,13,14,12,13,13,14,13,14,14,15,11,12,12,13,12,13,13,14,12,13,13,14,13,14,14,15,12,13,13,14,13,14,14,15,13,14,14,15,14,15,15,16]


healthSeq cur target | cur < target = let max_inc = (cur - 1 )  `quot` 10
                                      in (cur - 1 ) : ( healthSeq (cur + max_inc) target)
                     | otherwise = []

killPts = 11120
maxVal =  65535

staticWave = (healMax 10000 1 2) ++ concatMap (\i -> ( attack killPts 1 i (i+2)) ++  ( healMax (maxVal - killPts) 1 (i+2) )) [0 .. 255 ]

debugStat = (healMax 10000 1 2) ++ ( attack killPts 1 (255 - 33 ) 2 )

-- hugeWave = concatMap (\i -> ( healMax 10000 i (i+i) ) ++ (attack killPts i (i + i ) )) [0 .. 255 ]

-- bigWave = concatMap (\i -> ( healTo (10000 + killPts )  10000 i (i+i) ) ++ (attack killPts i (i + 1 ) )) [0 .. 255 ]


-- smallWave = concatMap (\i -> ( healTo (killPts +1 )  10000 i (i+i) ) ++ (attack killPts i (i + 1 ) )) [0 .. 255 ]



