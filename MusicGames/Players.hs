module Players where
import State
import Game
import Strategy
import Euterpea hiding (Player)
import Hagl

{- 

This module contains some sample players and common two-player pairings for an
Improvise game.

-}

--
-- * Sample players and preferences 
--

player1 :: SingularScore
player1 = SS [] [Begin (A,4), Extend (A, 4), Begin (G,4), Extend (G, 4),
                 Begin (F, 4), Extend (F, 4), Begin (G, 4), Extend (G, 4),
                 Begin (A, 4), Extend (A, 4), Begin (A, 4), Extend (A, 4),
                 Begin (A, 4), Extend (A, 4), Extend (A, 4), Extend (A, 4)]
player2 :: SingularScore
player2 = SS [] (replicate 16 (Begin (C, 4)))

player3 :: SingularScore
player3 = SS [] [Begin (D,4), Extend (D, 4), Begin (C, 4)]


start = RS [player1, player2] []

samplePrefs  ::[IntPreference]
samplePrefs = [(1, -1), (2, -1), (3, 0), (4 , 5), (5, 0), (6, -1), (7, 5), (8, -1), (9, -1), (10, -1), (11, -1), (12, 3)]

player1Prefs ::[IntPreference]
player1Prefs = [(8, -2), (3, -2)]
player2Prefs ::[IntPreference]
player2Prefs = []--[(5, 1), (3, 1)]


-- 
-- * Two-player pairings
--

scoreVsMiniMax :: [Player Improvise]
scoreVsMiniMax = [justTheScore, depthMiniMax]

miniVsMini :: [Player Improvise]
miniVsMini = [depthMiniMax, depthMiniMax]


--
-- * Players 
--

-- | A player who always plays the score and never deviates.
justTheScore :: Player Improvise
justTheScore = "Mr. Score" ::: myScore

-- | A player who looks ahead to play the optimal move, but only so far.
depthMiniMax :: Player Improvise
depthMiniMax = "Mr. Depth" ::: minimaxLimited 4 
    (intervalPayoff [player1Prefs, player2Prefs])

-- | A player playing the minimax strategy.
testMinimax :: Player Improvise
testMinimax  = "Mr. Minimax" ::: minimax

-- | A player who cycles through a list of set moves.
testPeriodic :: Player Improvise
testPeriodic = "Miss Periodic" ::: 
    periodic [Begin (C, 4), Begin (D, 4), Begin (E, 4), Begin (F, 4)]

