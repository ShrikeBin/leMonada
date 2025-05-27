module MontyHall where

import ProbabilisticMonad
import Control.Applicative -- liftA3

-- | Monty Hall Problem
-- | THIS NO WORKING SOLUTION
-- first maybe create a distribution of all possible outcomes
-- then filter base on if you choose to change or not...
condDist (<=6) . (uncurry (+)) (liftA3 (,) (die 2) (die 2) (die 2))