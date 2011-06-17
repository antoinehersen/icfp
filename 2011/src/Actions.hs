module Actions where

data ApplicationSide = LeftApp | RightApp


-- data Move =

instance Show ApplicationSide where
    show LeftApp  = "1"
    show RightApp = "2"