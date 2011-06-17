module Cards where

data Card = I
          | Zero
          | Succ
          | Dbl
          | Get
          | Put
          | S
          | K
          | Inc
          | Dec
          | Attack
          | Help
          | Copy
          | Revive
          | Zombie



instance Show Card where
    show I      = "I"
    show Zero   = "zero"
    show Succ   = "succ"
    show Dbl    = "dbl"
    show Get    = "get"
    show Put    = "put"
    show S      = "S"
    show K      = "K"
    show Inc    = "inc"
    show Dec    = "dec"
    show Attack = "attack"
    show Help   = "help"
    show Copy   = "copy"
    show Revive = "revive"
    show Zombie = "zombie"


readCard :: String -> Card
readCard  "I" = I
readCard    "zero" = Zero
readCard    "succ" = Succ
readCard     "dbl" = Dbl
readCard     "get" = Get
readCard     "put" = Put
readCard       "S" = S
readCard       "K" = K
readCard     "inc" = Inc
readCard     "dec" = Dec
readCard  "attack" = Attack
readCard    "help" = Help
readCard    "copy" = Copy
readCard  "revive" = Revive
readCard  "zombie" = Zombie
readCard s = error $ "Not a card: " ++ s

