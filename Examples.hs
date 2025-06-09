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
stochastic :: Int -> Dist Int
stochastic n = dedupeDist $ go 0 n
    where
        go pos 0 = return pos
        go pos m = do
            dir <- bernouli 0.5 (-1) 1
            go (pos + dir) (m - 1)

-- second attempt: non-recursive, still exponential
stochastic2 :: Int -> Dist Int
stochastic2 n = dedupeDist $ do
    steps <- replicateM n (bernouli 0.5 (-1) 1) -- expands to something like: step1 <- bernouli, step2 <- bernouli, step3 <- bernouli
    return (sum steps)

-- third attempt: we remember that stochastic process on Z
-- is a linear transformation of a binomial distribution
-- - polynomial time
stochasticBin :: Int -> Dist Int
stochasticBin n = fmap (\x -> 2 * x - n) (binomial n 0.5)

-- We can try to check if a given Distribution
-- is a known Distribution, and try to prove why we can
-- write stochastic process using binomial dist.
recognizeDist :: Dist a -> String
recognizeDist (Dist []) = error "Empty Distribution"
recognizeDist dist
    -- infinite - we're checking only a few first elements
    | isDist dist ((Dist . take 1000 . unpackDist) (geometric x)) = "Geometric"
    | isDist dist ((Dist . take 1000 . unpackDist) (negativeBinomial k p)) = "Negative Binomial"
    -- finite
    | isDist dist (bernouli x 0 1) = "Bernouli"
    | isDist dist (uniform [0 .. n]) = "Uniform"
    | isDist dist (binomial n (x ** (1 / fromIntegral n))) = "Binomial"
    | otherwise = "Unknown"
    where
        epsilon = 1e-8 -- floating point precision
        isDist (Dist as) (Dist bs) = foldl (\q ((_, a), (_,b)) -> q && (abs (a - b) < epsilon)) True (zip as bs)
        ((_,x):xs) = unpackDist dist
        k = (1+) $ length . takeWhile ((==0) . snd) $ unpackDist dist
        p = (1-) $ snd (unpackDist dist !! (k-1)) ** (1 / fromIntegral k)
        n = length xs

-- Check if a stochastic process is really a binomial distribution
example2 :: String
example2 = recognizeDist $ stochastic2 10 

-- Simulation of a station receiving messages with
-- a certain probability p.
-- Wait only n - time to not wait forever
station :: Prob -> Int -> Dist Int
station p n = Dist . take n . unpackDist $ try 1 n
    where
        try _ 0 = do return (n+1)
        try t k = do
            receive <- bernouli p True False
            if receive then return t
            else try (t+1) (k-1)

-- Check if station is picking up messages with geometric time
example3 :: String
example3 = recognizeDist $ station 0.3 10


-- Central Limit Theorem Example
-- presidential election: candidates N and T
-- We want to estimate fraction p of people supporting N
-- We ask randomly chosem people
-- We want to have probability >= 0.95 of being wrong by <= 0.05

-- p is true fraction of people supporting N
pollSample :: Int -> Prob -> Dist Int
pollSample n p = binomial n p

-- Using CLT approximation
confidenceInterval :: Int -> Int -> Prob -> (Prob, Prob)
confidenceInterval n success confidence = (lower, upper)
    where
        p' = fromIntegral success / fromIntegral n
        z = 1.96
        margin = z * sqrt (p' * (1 - p') / fromIntegral n)
        lower = max 0 (p' - margin)
        upper = min 1 (p' + margin)


-- returns distribution of calculated proportion
electionPoll :: Int -> Prob -> Dist Prob
electionPoll n trueP = do
    supporters <- pollSample n trueP
    let estimate = fromIntegral supporters / fromIntegral n
    return estimate

-- check if we're within 0.05 of true probability
checkAccuracy :: Int -> Prob -> Prob
checkAccuracy n trueP =
    evalDist (\estimate -> abs (estimate - trueP) <= 0.05) (electionPoll n trueP)

-- Try different values of n
findSampleSize :: Prob -> Int
findSampleSize trueP = head [n | n <- [10,20 .. 1000], checkAccuracy n trueP >= 0.95]

-- Search for sample size based on probability
example4 :: Double -> Int
example4 a = findSampleSize a

-- Show the distribution of p' based on true p and sample size
example5 :: Double -> Int -> Dist Prob
example5 x n = dedupeDist $ electionPoll n x

-- calculate theoretical sample size based on central limit theorem
theoreticalSampleSize :: Prob -> Int
theoreticalSampleSize trueP = ceiling $ (1.96^2 * trueP * (1 - trueP)) / (0.05^2)

-- Compare the sample size found by simulation and theoretical sample size
example6 :: Double -> (Int, Int)
example6 trueP = (findSampleSize trueP, theoreticalSampleSize trueP)


-- Weather forecaster
-- if today is sunny, then tomorrow is sunny with probability 0.6, cloudy: 0.3 rainy: 0.1
-- if today is cloudy, then tomorrow is sunny with probability 0.3, cloudy 0.4, rainy: 0.3
-- if today is rainy, then tomorrow is sunny with probability 0.2, cloudy: 0.3, rainy: 0.5
weather :: String -> Dist String
weather "sunny" = Dist [("sunny", 0.6), ("cloudy", 0.3), ("rainy", 0.1)]
weather "cloudy" = Dist [("sunny", 0.3), ("cloudy", 0.4), ("rainy", 0.3)]
weather "rainy" = Dist [("sunny", 0.2), ("cloudy", 0.3), ("rainy", 0.5)]

-- weather forecast
weatherForecast :: String -> Dist [String]
weatherForecast init_weather = do
    day1 <- weather init_weather
    day2 <- weather day1
    day3 <- weather day2
    return [init_weather, day1, day2, day3]

example7 :: String -> Dist [String]
example7 init_weather = weatherForecast init_weather

offspring :: Dist Int
offspring = Dist [ (0, 0.2), (1, 0.5), (2, 0.3)]

-- Population growth model after 2 generations
populationGrowth :: Dist Int
populationGrowth = do
    gen1 <- offspring
    gen2 <- case gen1 of
        0 -> return 0
        1 -> offspring
        2 -> do
            child1 <- offspring
            child2 <- offspring
            return (child1 + child2)
        _ -> return 0
    return gen2

-- Example of population growth
example8 :: Dist Int
example8 = populationGrowth