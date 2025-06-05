module Examples where

import ProbabilisticMonad
import Control.Applicative -- liftA2

-- Roll 2 six sided dices,
-- Probability that the first one is 2
-- Knowing that the sum is no greater than 5
example1 :: Prob
example1 = (evalDist ((==2) . fst)) ((condDist ((<= 5) . uncurry (+))) (liftA2 (,) (die 6) (die 6)))

-- Stochastic Processes
example2 :: Int -> Dist Int
example2 n = dedupeDist $ go 0 n
    where
        go pos 0 = return pos
        go pos n = do
            dir <- bernouli 0.5 (-1) 1
            go (pos + dir) (n - 1)
