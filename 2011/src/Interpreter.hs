module Interpreter where

import Cards
import Actions
import Data.Array

import Control.Monad
import Control.Monad.State

import Debug.Trace

-- F function, Par paritial application
data Field = Val Int | F Card | Par Field Field | Error deriving (Eq, Show)

data Slot = Slot { field :: Field, vitality :: Int} deriving (Eq, Show)
type Slots = Array Int Slot
data World = World { proponent :: Slots , opponent :: Slots } deriving Show




defaultSlot = Slot (F I) 10000
defaultSlots = listArray (0, 255) (repeat defaultSlot)
defaultWorld = World defaultSlots defaultSlots




filterSlots slots = let ls = assocs slots
                        test (_, f) = f /= defaultSlot
                    in
                      filter test ls

showWorld (World pro opp) = "Proponent: " ++ show (filterSlots pro)
                            ++ "\nOpponent: " ++ show (filterSlots opp)



putOpp opp = do World pro _ <- get
                put $ World pro opp

getOpp :: State World Slots
getOpp = do World _ opp <- get
            return opp

putPro pro = do World _ opp <- get
                put $ World pro opp


getPro :: State World Slots
getPro = do World pro _ <- get
            return pro


getVitality slots = map vitality $ elems slots

cardToField Zero = Val 0
cardToField card = F card

normalizeVal i | i > 65535 = 65535
               | otherwise = i

validIdx i = i >= 0 && i <= 255

isAlive slots i = vitality (slots ! i ) > 0
isDead slots i = not $ isAlive slots i





data Inter t = Run t Int | IError deriving (Show)

instance Monad Inter where
  return a = Run a 0
  (>>=) (Run a i) f | i > 2000 = trace "max rec" IError
                    | otherwise = case (f a) of
                                    Run r j | (i+j + 1) > 2000 -> trace "max rec" IError
                                            | otherwise -> Run r (i+j + 1)
                                    IError -> IError
  (>>=)  IError _ = IError
  fail _ = IError



increaseVit slots idx i = let slot = slots ! idx
                              cur_vit = vitality slot
                              new_vit = min 65535 (cur_vit + i)
                          in
                            case cur_vit of
                              _ | cur_vit > 0 && cur_vit < 65535 -> slots // [ (idx, slot { vitality = new_vit } ) ]
                                | otherwise -> slots

decreaseVit slots idx i = let slot = slots ! idx
                              cur_vit = vitality slot
                              new_vit = max 0 (cur_vit -i)
                          in
                            case cur_vit of
                              _ | cur_vit > 0 -> slots // [ (idx, slot { vitality = new_vit } ) ]
                                | otherwise   -> slots

-- TODO fix number of application
applyS f g x = do h <- applyFun f x
                  case h of
                    Error -> return $ Error
                    _ -> do y <- applyFun g x
                            case y of
                              Error -> return $ Error
                              _ -> applyFun h y

applyAttack i j n = do prop <- getPro
                       let v = vitality $ prop ! i
                       if n > v then
                          return Error
                       else do
                         pushPro $ decreaseVit prop i n
                         case j of
                           Val jdx | validIdx jdx -> do let dec = quot (9 * n ) 10
                                                        opp <- getOpp
                                                        putOpp $ decreaseVit opp (255 - jdx) dec
                                                        return $ F I
                           _ -> return Error

applyHelp prop opp i j n = let v = vitality $ prop ! i
                               new_prop = decreaseVit prop i n
                           in
                             if n > v then
                                 (Error, prop, opp)
                             else
                                 case j of
                                   Val jdx | validIdx jdx -> let inc = quot (11 * n ) 10
                                                                 new_new_prop = increaseVit new_prop jdx inc
                                                             in ( F I , new_new_prop, opp)
                                   _ -> (Error, new_prop, opp)

revive prop i = let slot = prop ! i
                in
                  if vitality slot <= 0 then
                      prop // [ (i, slot {vitality = 1} ) ]
                  else
                      prop


maxVal = 65535

applyFun leftF rightF =
    case (leftF, rightF) of
      (F I, x ) -> return x
      ( F Succ, Val i ) -> return $ Val $ max (i+1) maxVal
      ( F Dbl , Val i ) -> return $ Val $ max (i*2) maxVal
      ( F Get , Val i ) | validIdx i -> do prop <- getPro
                                           return  $ field $ prop ! i
      ( F Put , _ ) -> return $ F I
      ( F S, f) -> return $ Par (F S) f
      ( x@(Par (F S) _ ) , g ) -> return $ Par x g
      ( Par (Par (F S) f ) g , x ) -> applyS f g x
      ( F K , x ) -> return $ Par (F K) x
      ( Par (F K) x, _ ) -> return x
      ( F Inc, Val idx) | validIdx idx -> do prop <- getPro
                                             let new_prop = increaseVit prop idx 1
                                             putPro new_prop
                                             return $ F I
      ( F Dec, Val idx) | validIdx idx -> do opp <- getOpp
                                             let new_opp = decreaseVit opp (255-idx) 1
                                             putOpp new_opp
                                             return $ F I
      ( F Attack, i) -> return $ P (F Attack) i
      ( a@(P (F Attack) _) , j ) -> return $ P a j
      ( P ( P (F Attack) (Val i)) j , Val n ) | validIdx i -> applyAttack i j n
                                            ( F Help , i ) -> ( PartialF Help [i], prop, opp)
                                            ( PartialF Help [i], j) -> (PartialF Help [i,j], prop, opp)
                                            ( PartialF Help [Val i, j ], Val n ) | validIdx i -> applyHelp prop opp i j n
                                            ( F Copy, Val i) | validIdx i -> ( field $ opp ! i, prop , opp )
                                            ( F Revive, Val i ) | validIdx i -> ( F I, revive prop i, opp )
                                            ( F Zombie, i ) -> (PartialF Zombie [i], prop, opp)
                                            ( PartialF Zombie [Val i], x ) | validIdx i && isDead opp i -> ( F I, prop, opp // [(i, Slot x (-1) )] )
                                            _ -> (Error, prop, opp)


cleanRes Error = trace " !! Error !! " F I
cleanRes f = f

updateProponent :: World -> Move -> World
updateProponent w@(World prop opp) (Move side card idx)
    = let fun = cardToField card
          slot = prop ! idx
          -- TODO check if dead
          eval = case side of
                   LeftApp  -> applyFun fun ( field slot)
                   RightApp -> applyFun (field slot) fun
          (tmp_res, World newProp newOpp) = runState eval w
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



