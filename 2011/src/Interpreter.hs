module Interpreter where

import Cards
import Actions
import Data.Array

import Debug.Trace

data Field = Val Int | Func Card | PartialF Card [Field] | Error deriving (Eq, Show)

data Slot = Slot { field :: Field, vitality :: Int} deriving (Eq, Show)

type Slots = Array Int Slot

data World = World { proponent :: Slots , opponent :: Slots } deriving Show

defaultSlot = Slot (Func I) 10000

defaultSlots = listArray (0, 255) (repeat defaultSlot)

defaultWorld = World defaultSlots defaultSlots

filterSlots slots = let ls = assocs slots
                        test (_, f) = f /= defaultSlot
                    in
                      filter test ls

showWorld (World pro opp) = "Proponent: " ++ show (filterSlots pro)
                            ++ "\nOpponent: " ++ show (filterSlots opp)

-- TODO this should really be a monad !

cardToFunc Zero = Val 0
cardToFunc card = Func card

normalizeVal i | i > 65535 = 65535
               | otherwise = i

validIdx i = i >= 0 && i <= 255

isAlive slots i = vitality (slots ! i ) > 0
isDead slots i = not $ isAlive slots i


increaseVit slots idx i = let slot = slots ! idx
                              cur_vit = vitality slot
                              new_vit = min 65535 (cur_vit + i)
                          in
                            case cur_vit of
                              _ | cur_vit > 0 && cur_vit < 65535 -> slots // [ (idx, slot { vitality = new_vit } ) ]
                              otherwise -> slots

decreaseVit slots idx i = let slot = slots ! idx
                              cur_vit = vitality slot
                              new_vit = max 0 (cur_vit -i)
                          in
                            case cur_vit of
                              _ | cur_vit > 0 -> slots // [ (idx, slot { vitality = new_vit } ) ]
                              otherwise -> slots

applyS prop opp f g x = let ( h, prop1, opp1) = applyFun prop opp f x
                            ( y, prop2, opp2) = applyFun prop1 opp1 g x
                        in
                          applyFun prop2 opp2 h y

-- ! use lazyness
applyAttack prop opp i j n = let v = vitality $ prop ! i
                                 new_prop = decreaseVit prop i n
                             in
                               if n > v then
                                   (Error, prop, opp)
                               else
                                   case j of
                                     Val jdx | validIdx jdx -> let dec = quot (9 * n ) 10
                                                                   new_opp = decreaseVit opp (255 - jdx) dec
                                                               in ( Func I , new_prop, new_opp)
                                     _ -> (Error, new_prop, opp)

applyHelp prop opp i j n = let v = vitality $ prop ! i
                               new_prop = decreaseVit prop i n
                           in
                             if n > v then
                                 (Error, prop, opp)
                             else
                                 case j of
                                   Val jdx | validIdx jdx -> let inc = quot (11 * n ) 10
                                                                 new_new_prop = increaseVit new_prop jdx inc
                                                             in ( Func I , new_new_prop, opp)
                                   _ -> (Error, new_prop, opp)

revive prop i = let slot = prop ! i
                in
                  if vitality slot <= 0 then
                      prop // [ (i, slot {vitality = 1} ) ]
                  else
                      prop


applyFun prop opp leftF rightF = case (leftF, rightF) of
                                   ( Func I, x ) ->  ( x , prop, opp)
                                   ( Func Succ, Val i ) -> ( Val $ normalizeVal (i+1), prop , opp)
                                   ( Func Dbl , Val i ) -> ( Val $ normalizeVal (i*1), prop , opp)
                                   ( Func Get , Val i ) | validIdx i-> ( field $ prop ! i , prop , opp )
                                   ( Func Put , _ ) -> ( Func I , prop, opp)
                                   ( Func S, f) -> ( PartialF S [f] , prop, opp )
                                   ( PartialF S [f] , g ) -> ( PartialF S [f, g ] , prop, opp )
                                   ( PartialF S [f,g], x ) -> applyS prop opp f g x
                                   ( Func K , x ) -> ( PartialF K [x] , prop, opp )
                                   ( PartialF K [x] , y ) -> ( x , prop , opp )
                                   ( Func Inc, Val idx) | validIdx idx  -> (Func I, increaseVit prop idx 1, opp)
                                   ( Func Dec, Val idx) | validIdx idx ->  (Func I, prop, decreaseVit opp (255-idx) 1 )
                                   ( Func Attack, i) -> (PartialF Attack [i] , prop, opp)
                                   ( PartialF Attack [i], j ) -> (PartialF Attack [i,j] , prop , opp)
                                   ( PartialF Attack [Val i, j], Val n ) | validIdx i -> applyAttack prop opp i j n
                                   ( Func Help , i ) -> ( PartialF Help [i], prop, opp)
                                   ( PartialF Help [i], j) -> (PartialF Help [i,j], prop, opp)
                                   ( PartialF Help [Val i, j ], Val n ) | validIdx i -> applyHelp prop opp i j n
                                   ( Func Copy, Val i) | validIdx i -> ( field $ opp ! i, prop , opp )
                                   ( Func Revive, Val i ) | validIdx i -> ( Func I, revive prop i, opp )
                                   ( Func Zombie, i ) -> (PartialF Zombie [i], prop, opp)
                                   ( PartialF Zombie [Val i], x ) | validIdx i && isDead opp i-> ( Func I, prop, opp // [(i, Slot x (-1) )] )
                                   _ -> (Error, prop, opp)


cleanRes Error = trace " !! Error !! " Func I
cleanRes field = field

updateProponent :: World -> Move -> World
updateProponent (World prop opp) (Move side card idx) = let fun = cardToFunc card
                                                            slot = prop ! idx
-- TODO check if dead
                                                            (tmp_res, newProp, newOpp) = case side of
                                                                                       LeftApp  -> applyFun prop opp fun ( field slot)
                                                                                       RightApp -> applyFun prop opp (field slot) fun
                                                            res = cleanRes tmp_res
                                                            new_slot = slot { field = res }
                                                                       -- trace ("fun: " ++ show fun
                                                                       --                ++ " slot: " ++ show slot
                                                                       --                ++ " tmp_res: " ++ show tmp_res
                                                                       --                ++ " res: " ++ show res ) 
                                                            finalProp = newProp // [ (idx, new_slot) ]
                                                        in
                                                          World finalProp newOpp

updateOpponent :: World -> Move -> World
updateOpponent (World prop opp) move = World new_opp new_prop
    where
      (World new_prop new_opp) = updateProponent (World opp prop) move



