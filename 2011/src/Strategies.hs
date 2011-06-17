module Strategies where

import Cards
import Actions

strategyDoNothing = repeat idleMove

strategySimpleAttack = cycle [ Move RightApp Dec 0, Move RightApp Zero 0 ]