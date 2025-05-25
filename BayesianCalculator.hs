module BayesianCalculator where

import ProbabilisticMonad

bayesMedicalTest = (evalDist fst . condDist snd) ( do
    hasDisease <- bernouli 0.01 True False
    testPositive <-
        if hasDisease
            then bernouli 0.95 True False
            else bernouli 0.05 True False
    return (hasDisease, testPositive))

