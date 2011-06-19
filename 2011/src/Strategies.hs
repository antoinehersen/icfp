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

avoidColisionWith i j | i /= j = i
                      | otherwise = (i + 7) `mod` 256


reviveStr target' from' = let target = target' `mod` 256
                              from = avoidColisionWith ( from' `mod` 256 ) target
                          in
                            clean from ++ setNb target from ++ [ Move LeftApp Revive from ]


zipRound range = zip range ( tail $ cycle range )

reviveWave targets = concatMap (\(t,f) -> reviveStr t f) (zipRound targets)


------------------------------------------------
------------------------------------------------
------------------------------------------------

setNb = nbToMoves

applyNb nb = applyNbMoves nb
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

heal pts target' i' = let target = target' `mod` 256
                          i = avoidColisionWith ( i' `mod` 256 ) target
                      in
                        clean i ++ [ Move RightApp Help i ]
                                  ++ optimalArg target target i ++ optimalArg target target i ++ optimalArg target pts i

healTo to from target i = let seq = healthSeq from to
                          in concatMap (\pts -> heal pts target i ) seq

healMax = healTo 65535


attack pts base' target' i' = let target = target' `mod` 256
                                  base = base'  `mod` 256
                                  i = avoidColisionWith ( i' `mod` 256 ) base
                      in clean i ++ [ Move RightApp Attack i ]
                             ++ optimalArg base base i ++ optimalArg base target i ++ optimalArg base pts i

healthSeq cur target | cur < target = let max_inc = (cur - 1 )  `quot` 10
                                      in (cur - 1 ) : ( healthSeq (cur + max_inc) target)
                     | otherwise = []

killPts = 11120
maxVal =  65535

halfKill = 6000

staticWave = (healMax 10000 1 2) ++ concatMap (\i -> ( attack killPts 1 i (i+2)) ++  ( healMax (maxVal - killPts) 1 (i+2) )) [0 .. 255 ]

maxAttack base targets =
    (healMax 10000 base (base + 1))
    ++ concatMap (\i -> ( attack killPts base (255 - i) (i+1)) ++  ( healMax (maxVal - killPts) base (i+1) )) targets


sniper target sac1 sac2 i = (attack halfKill sac1 (255 - target) i) ++ (attack halfKill sac2 (255 - target) i)
snipeTarg targets = concatMap (\t ->  sniper t (t*2 + 10 ) (t*2 + 11 ) 9 )  targets

debugStat = sniper 0 12 13 3

-- hugeWave = concatMap (\i -> ( healMax 10000 i (i+i) ) ++ (attack killPts i (i + i ) )) [0 .. 255 ]

-- bigWave = concatMap (\i -> ( healTo (10000 + killPts )  10000 i (i+i) ) ++ (attack killPts i (i + 1 ) )) [0 .. 255 ]


-- smallWave = concatMap (\i -> ( healTo (killPts +1 )  10000 i (i+i) ) ++ (attack killPts i (i + 1 ) )) [0 .. 255 ]


-- 32640
modRange modulo range  =  [ (x*modulo ) `mod` 256 | x <- range ]
modFullRange modulo = modRange modulo [0 .. 255 ]

sumW i = sum [ (x*i) `mod` 256 | x <- [0 .. 255 ]]

possibleWaveLength = [ x | x <- [0..255] , sumW x == 32640 ]

-- [1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91,93,95,97,99,101,103,105,107,109,111,113,115,117,119,121,123,125,127,129,131,133,135,137,139,141,143,145,147,149,151,153,155,157,159,161,163,165,167,169,171,173,175,177,179,181,183,185,187,189,191,193,195,197,199,201,203,205,207,209,211,213,215,217,219,221,223,225,227,229,231,233,235,237,239,241,243,245,247,249,251,253,255]


-- *Strategies> map (length .nbToCards)  [0 .. 255]
-- [1,2,3,4,4,5,5,6,5,6,6,7,6,7,7,8,6,7,7,8,7,8,8,9,7,8,8,9,8,9,9,10,7,8,8,9,8,9,9,10,8,9,9,10,9,10,10,11,8,9,9,10,9,10,10,11,9,10,10,11,10,11,11,12,8,9,9,10,9,10,10,11,9,10,10,11,10,11,11,12,9,10,10,11,10,11,11,12,10,11,11,12,11,12,12,13,9,10,10,11,10,11,11,12,10,11,11,12,11,12,12,13,10,11,11,12,11,12,12,13,11,12,12,13,12,13,13,14,9,10,10,11,10,11,11,12,10,11,11,12,11,12,12,13,10,11,11,12,11,12,12,13,11,12,12,13,12,13,13,14,10,11,11,12,11,12,12,13,11,12,12,13,12,13,13,14,11,12,12,13,12,13,13,14,12,13,13,14,13,14,14,15,10,11,11,12,11,12,12,13,11,12,12,13,12,13,13,14,11,12,12,13,12,13,13,14,12,13,13,14,13,14,14,15,11,12,12,13,12,13,13,14,12,13,13,14,13,14,14,15,12,13,13,14,13,14,14,15,13,14,14,15,14,15,15,16]

finalStrategy = snipeTarg [0..7] ++ maxAttack 1 [ 8 .. 255 ] ++ reviveWave (modFullRange 131 )
                ++ maxAttack 7 (take 60 (modFullRange 79 ) )
                ++ concatMap ( \m -> reviveWave (modFullRange m )) [37, 77, 247]