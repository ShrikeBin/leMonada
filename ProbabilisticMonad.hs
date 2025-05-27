module ProbabilisticMonad where

import qualified Data.Map as M
import Text.Printf (printf)
import Control.Applicative


-- Type Declarations --
-- |===================================================================================| --
-- Percentage as Double --
type Prob = Double

-- Distribution type is a list of (Object, Probability) --
newtype Dist a = Dist[(a, Prob)]
-- |===================================================================================| --


-- Helper Functions ---
-- |===================================================================================| --
-- Take a probalility from distribution --
unpackDist :: Dist a -> [(a, Prob)]
unpackDist (Dist xs) = xs

-- Remove duplicates --
-- (Make a map, sume values of duplicates) --
dedupeDist :: (Ord a) => Dist a -> Dist a
dedupeDist (Dist xs) = Dist (M.toList (M.fromListWith (+) xs))

-- Self Explanatory --
sumProb :: [(a, Prob)] -> Prob
sumProb xs = sum (map snd xs)

-- Normalize to 1.0 --
normProb :: [(a, Prob)] -> [(a, Prob)]
normProb xs = [(x, p / q) | let q = sumProb xs, (x,p) <- xs]
-- |===================================================================================| --


-- Basic type operations ---
-- |===================================================================================| --
-- Show in a pretty way --
instance (Show a, Ord a) => Show (Dist a) where
  show dist = concatMap showRow ( (unpackDist . dedupeDist) dist)
    where
      showRow (elem, prob) = padded elem ++ " | " ++ printf "%.12f" prob ++ "\n"
      padded elem = replicate (maxElemLen - (length . show) elem) ' ' ++ show elem
      maxElemLen = maximum (map (length . show . fst) (unpackDist dist))

-- Time to do the funny --
instance Functor Dist where
    fmap f (Dist xs) = Dist (map (\(x, p) -> (f x, p)) xs)

-- Time to do the funnier --
instance Applicative Dist where
    pure x = Dist [(x, 1.0)]

    -- Apply --
    (Dist functions) <*> (Dist elements) = Dist ( do
        (x, px) <- elements
        (f, pf) <- functions
        return (f x, px * pf))

-- Time to do the funniest --
instance Monad Dist where
    return = pure

    -- (>>=) :: Dist a -> (a -> Dist b) -> Dist b
    (Dist xs) >>= f = Dist ( do
        (x, px) <- xs
        (y, py) <- unpackDist (f x)
        return (y, px * py))
-- |===================================================================================| --


-- DISTRIBUTIONS and TESTERS --
-- |===================================================================================| --
uniform :: [a] -> Dist a
uniform xs = Dist . normProb $ map (,1.0) xs

bernouli :: Prob -> a -> a -> Dist a
bernouli p x y
    | p < 0.0 || p > 1.0 = error "p ∉ [0, 1.0]"
    | otherwise = Dist [(x, p), (y, 1 - p)]

-- Sum n tries from left for example --
binomial :: Int -> Prob -> Dist Int
binomial n p  = foldl1 (\x y -> dedupeDist (liftA2 (+) x y)) (replicate n (bernouli p 1 0))

-- The number of trials until the first success  (Infinite so be carefull) --
geometric :: Prob -> Dist Int
geometric p
    | p < 0.0 || p > 1.0 = error "p ∉ [0, 1.0]"
    | otherwise = Dist [(n, (1 - p) ^ (n - 1) * p) | n <- [1 ..]]

-- Negative Binomial (Also Infinite) --
negativeBinomial :: Int -> Prob -> Dist Int
negativeBinomial k p = foldl1 (\x y -> dedupeDist (liftA2 (+) x y)) (replicate k (geometric p))

-- Poisson Distribution --
poisson :: Double -> Dist Int
poisson λ
    | λ < 0 = error "λ must be +"
    | otherwise = Dist [(k, (exp (-λ) * (λ ** fromIntegral k)) / fromIntegral (factorial k)) | k <- [0 ..]]
    where
      factorial n = product [1 .. n] `max` 1

-- Fair, n'side Die
die :: Int -> Dist Int
die n = uniform [1 .. n]
-- |===================================================================================| --


-- CRUCIAL FUNCTIONS TO OPERATE ON PROBABILTY --
-- |===================================================================================| --
-- A way of desciribing events --
-- Function that says if something is true (will be clear in a second) --
type Event a = a -> Bool

-- Sum probabilities of a given event --
-- [example: evalDist (==2) (die 6)]
-- [This will return the probability of rolling a 2 on a fair die]
evalDist :: Event a -> Dist a -> Prob
evalDist eventFunc = sumProb . filter (eventFunc . fst) . unpackDist

-- Conditional Distribution with a condition function --
-- We have a Distribution, we apply a condtion on it, and normalize again --
-- [example condDist (==2) (die 6)]
-- [This will return a distribution of rolling a 2 on a fair die, which is just 1.0]
condDist :: (a -> Bool) -> Dist a -> Dist a
condDist f (Dist xs) = (Dist . normProb) (filter (f . fst) xs)
-- |===================================================================================| --
