module MontyHall where

import ProbabilisticMonad

-- | Monty Hall Problem
-- | It's a probablistic model of the game show's mechanics,
-- | that assumes choosing the door from a uniform distribution.

-- | Under the hood: montyHall evaluates all possible outcomes
-- | of the game using list comprehensions and multiplies
-- | conditional probabilities.
-- | Use of list comprehension arises from their implicit use
-- | in the (>>=) operator.

montyHall :: Dist String
montyHall = dedupeDist $ do
    let doors = [1, 2, 3]
    win <- uniform doors
    pick <- uniform doors
    open <- uniform $ filter (\door -> door /= pick && door /= win) doors
    let switch = head $ filter (\door -> door /= pick && door /= open) doors -- same as: switch <- pure $ head [...]
    return $ if switch == win
        then "switch"
        else "stay"
