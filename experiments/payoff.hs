import System.Random

goodness:: Float -> IO Float
goodness x = getStdRandom (randomR (0,x))

payoff:: Float -> Float -> Float-> Float
payoff my_goodness summed_goodness collective = collective * my_goodness / summed_goodness


