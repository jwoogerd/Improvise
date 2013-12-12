module Players where

import Game
import Strategy

import Euterpea hiding (Player, Performance)
import Hagl

{- 

This module contains some sample players and common two-player pairings for an
Improvise game.

-}

--
-- * Players 
--

-- | A player who always plays the score and never deviates.
justTheScore :: Player Improvise
justTheScore = "Mr. Score" ::: myScore

-- | A player who looks ahead to play the optimal move, but only so far.
depthMiniMax :: (Performance -> Payoff) -> Player Improvise
depthMiniMax pay = "Mr. Depth" ::: minimaxLimited 4 pay

-- | A player who plays every note in her score shifted by the given number of 
-- half steps. 
shifter :: Int -> Player Improvise
shifter i = "Dr. Shifty" ::: shiftScore i

-- | A player who uses the best n strategy with n = 3
maximize :: (Performance -> Payoff) -> Player Improvise
maximize pay = "Missus Maxine" ::: bestNLimited 3 4 pay

-- | A player who plays randomly from the possible moves.
randy :: Player Improvise
randy = "Sir Randy" ::: randomly


-- 
-- * Two-player pairings
--

-- | Play depth-limited minimax against a player who just plays the score.
scoreVsMiniMax :: (Performance -> Payoff) -> [Player Improvise]
scoreVsMiniMax pay = [justTheScore, depthMiniMax pay]

-- | Play depth-limited minimax against itself.
miniVsMini :: (Performance -> Payoff) -> [Player Improvise]
miniVsMini pay = [depthMiniMax pay, depthMiniMax pay]

-- | Play best 3 against best 3
bestVsBest :: (Performance -> Payoff) -> [Player Improvise]
bestVsBest pay = [maximize pay, maximize pay]

-- | Play score against best 3
scoreVsBest :: (Performance -> Payoff) -> [Player Improvise]
scoreVsBest pay = [justTheScore, maximize pay]

-- | Play two score-shifter against each other.
shiftVsShift :: [Player Improvise]
shiftVsShift = [shifter 4, shifter 4]

