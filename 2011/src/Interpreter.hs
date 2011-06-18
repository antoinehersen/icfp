module Interpreter where

import Cards
import Actions
import Data.Array

data Field = Val Int | Func Card | PartialF Card [Field] | Error deriving Show

data Slot = Slot { field :: Field, vitality :: Int} deriving Show

type Slots = Array Int Slot

data World = World { proponent :: Slots , opponent :: Slots } deriving Show


-- TODO this should really be a monad !

cardToFunc Zero = Val 0
cardToFunc card = Func card

normalizeVal i | i > 65535 = 65535
               | otherwise = i

validIdx i = i >= 0 && i <= 255

increaseVit slots idx i = let slot = slots ! idx
                              cur_vit = vitality slot
                          in
                            case cur_vit of
                              _ | cur_vit > 0 && cur_vit < 65535 -> slots // [ (idx, slot { vitality = (cur_vit + i) } ) ]
                              otherwise -> slots

decreaseVit slots idx i = let slot = slots ! idx
                              cur_vit = vitality slot
                          in
                            case cur_vit of
                              _ | cur_vit > 0 -> slots // [ (idx, slot { vitality = (cur_vit - i) } ) ]
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
                                     Val jdx | validIdx jdx -> let w = vitality $ opp ! (255 - jdx )
                                                                   dec = min w $ quot (9 * n ) 10
                                                                   new_opp = decreaseVit opp (255 - jdx) dec
                                                               in ( Func I , new_prop, new_opp)
                                     _ -> (Error, new_prop, opp)

applyFun prop opp leftF rightF = case (leftF, rightF) of
                                   ( Func I, x ) -> ( x , prop, opp)
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
                                   ( Func Dec, Val idx) | validIdx idx -> (Func I, prop, decreaseVit opp (255-idx) 1 )
                                   ( Func Attack, i) -> (PartialF Attack [i] , prop, opp)
                                   ( PartialF Attack [i], j ) -> (PartialF Attack [i,j] , prop , opp)
                                   ( PartialF Attack [Val i, j], Val n ) | validIdx i -> applyAttack prop opp i j n
                                   _ -> (Error, prop, opp)


cleanRes Error = Func I
cleanRes field = field

updateProponent :: World -> Move -> World
updateProponent (World prop opp) (Move side card idx) = let fun = cardToFunc card
                                                            slot = prop ! idx
                                                            (tmp_res, newProp, newOpp) = case side of
                                                                                       LeftApp  -> applyFun prop opp fun ( field slot)
                                                                                       RightApp -> applyFun prop opp (field slot) fun
                                                            res = cleanRes tmp_res
                                                            new_slot = slot { field = res }
                                                            finalProp = newProp // [ (idx, new_slot) ]
                                                        in
                                                          World finalProp newOpp