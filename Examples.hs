module Examples where

import ProbabilisticMonad
import Control.Applicative -- liftA2
import Control.Monad (replicateM)

-- Roll 2 six sided dices,
-- Probability that the first one is 2
-- Knowing that the sum is no greater than 5
example1 :: Prob
example1 = (evalDist ((==2) . fst)) ((condDist ((<= 5) . uncurry (+))) (liftA2 (,) (die 6) (die 6)))

-- Stochastic Processes
-- first attempt: recursive call through decision tree
-- rebranching each time - exponential time
example2 :: Int -> Dist Int
example2 n = dedupeDist $ go 0 n
    where
        go pos 0 = return pos
        go pos m = do
            dir <- bernouli 0.5 (-1) 1
            go (pos + dir) (m - 1)

-- second attempt: non-recursive, still exponential
example3 :: Int -> Dist Int
example3 n = dedupeDist $ do
    steps <- replicateM n (bernouli 0.5 (-1) 1) -- expands to something like: step1 <- bernouli, step2 <- bernouli, step3 <- bernouli
    return (sum steps)

-- third attempt: we remember that stochastic process on Z
-- is a linear transformation of a binomial distribution
-- - polynomial time
example4 :: Int -> Dist Int
example4 n = fmap (\x -> 2 * x - n) (binomial n 0.5)

-- We can try to check if a given Distribution
-- is a known Distribution, and try to prove why we can
-- write example4 using binomial dist.
example5 :: Dist a -> String
example5 (Dist []) = error "Empty Distribution"
example5 dist
    | isDist dist (bernouli x 0 1) = "Bernouli"
    -- infinite - we're checking only a few first elements
    | isDist dist ((Dist . take 10000 . unpackDist) (geometric x)) = "Geometric"
    -- | isDist dist ((Dist . take 10000 . unpackDist) (negativeBinomial ))
    -- finite
    | isDist dist (uniform [0 .. n]) = "Uniform"
    | isDist dist (binomial n (x ** (1 / fromIntegral n))) = "Binomial"
    | otherwise = "Unknown"
    where
        isDist (Dist as) (Dist bs) = foldl (\q ((_, a), (_,b)) -> q && (a == b)) True (zip as bs)
        ((_,x):xs) = unpackDist dist
        n = length xs
