module MontyHall where

import ProbabilisticMonad

-- | Monty Hall Problem
-- | IDK WHY IT WORK YET
doors = [1, 2, 3] :: [Int]

montyHallSwitch :: Prob
montyHallSwitch = evalDist id $ do
    car <- uniform doors
    pick <- uniform doors
    open <- uniform (filter (\door -> door /= pick && door /= car) doors)
    let switch = head (filter (\door -> door /= pick && door /= open) doors)
    return (switch == car) -- Probability of winning if you switch

montyHallStay :: Prob
montyHallStay = evalDist id $ do
    car <- uniform doors
    pick <- uniform doors
    return (pick == car) -- Probability of winning if you stay