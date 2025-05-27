module BayesianCalculator where

import ProbabilisticMonad

-- | Bayesian Calculator
-- | p - probability of Patient sick, Test True
-- | q - probability of Patien is actually sick
-- | Returns the probability of Patient is actually sick given that Test is True
bayesMedicalTest :: Prob -> Prob -> Prob
bayesMedicalTest p q
    | p < 0.0 || p > 1.0 = error "p ∉ [0, 1.0]" 
    | q < 0.0 || q > 1.0 = error "q ∉ [0, 1.0]"
    | otherwise = (evalDist fst . condDist snd) ( do
        hasDisease <- bernouli q True False
        testPositive <-
            if hasDisease
                then bernouli p True False
                else bernouli (1 - p) True False
        return (hasDisease, testPositive))

