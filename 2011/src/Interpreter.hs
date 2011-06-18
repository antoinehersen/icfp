module Interpreter where

import Cards
import Actions
import Data.Array

data Field = Val Int | Func Card | PartialF Card [Field] | Error deriving Show

data Slot = Slot { field :: Field, vitality :: Int} deriving Show

type Slots = Array Int Slot

data World = World { proponent :: Slots , opponent :: Slots } deriving Show


cardToFunc Zero = Val 0
cardToFunc card = Func card

normalizeVal i | i > 65535 = 65535
               | otherwise = i

applyS prop opp f g x = let ( h, prop1, opp1) = applyFun prop opp f x
                            ( y, prop2, opp2) = applyFun prop1 opp1 g x
                        in
                          applyFun prop2 opp2 h y

applyFun prop opp leftF rightF = case (leftF, rightF) of
                                   ( Func I, x ) -> ( x , prop, opp)
                                   ( Func Succ, Val i ) -> ( Val $ normalizeVal (i+1), prop , opp)
                                   ( Func Dbl , Val i ) -> ( Val $ normalizeVal (i*1), prop , opp)
                                   ( Func Get , Val i ) | i >= 0 && i <= 255 -> ( field $ prop ! i , prop , opp )
                                   ( Func Put , _ ) -> ( Func I , prop, opp)
                                   ( Func S, f) -> ( PartialF S [f] , prop, opp )
                                   ( PartialF S [f] , g ) -> ( PartialF S [f, g ] , prop, opp )
                                   ( PartialF S [f,g], x ) -> applyS prop opp f g x
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