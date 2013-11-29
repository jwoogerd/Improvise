
Module PureHagl.Examples.RPS where

import PureHagl

--
-- * Game representaion
--

-- | Moves in rock-paper-scissors
data RPS = Rock | Paper | Scissors deriving (Enum, Eq, Show)

{-
rps = square [Rock .. Scissors] [0,-1, 1,
                          1, 0,-1,
                         -1, 1, 0]
-}

--
-- * Players 
--

rocky :: Player RPS
rocky = Player "Rocky" (\s -> Rock)  

--playRPS = execGame rps
