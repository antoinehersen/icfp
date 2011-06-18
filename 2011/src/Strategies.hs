module Strategies where

import Cards
import Actions

strategyDoNothing:: [Move]
strategyDoNothing = repeat idleMove

strategySimpleAttack :: [Move]
strategySimpleAttack = cycle [ Move RightApp Dec 0, Move RightApp Zero 0 ]