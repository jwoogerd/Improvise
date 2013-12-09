module Players where
import Game
import Strategy
import Payoff
import Euterpea hiding (Player)
import Hagl

{- 

This module contains some sample players and common two-player pairings for an
Improvise game.

-}


pickPlayer :: String -> IO (Player Improvise)
pickPlayer file = do putStrLn ("Enter a name for the player of " ++ show file)
                     name <- readLn
                     strat <- pickStrategy                    
                     return (name ::: strat)

--
-- * Sample players and preferences 
--

-- | The opening notes to "Mary Had A Little Lamb".
mary :: Performer
mary = Performer [] [Begin (A,4), Extend (A, 4), Begin (G,4), Extend (G, 4),
                 Begin (F, 4), Extend (F, 4), Begin (G, 4), Extend (G, 4),
                 Begin (A, 4), Extend (A, 4), Begin (A, 4), Extend (A, 4),
                 Begin (A, 4), Extend (A, 4), Extend (A, 4), Extend (A, 4)]

-- | A complement to "Mary Had A Little Lamb"; play just C notes.
justCNotes :: Performer
justCNotes = Performer [] (replicate 16 (Begin (C, 4)))

-- ** Some sample preferences
--

samplePrefs  ::[IntPreference]
samplePrefs = [(1, -1), (2, -1), (3, 0), (4 , 5), (5, 0), (6, -1), (7, 5), 
    (8, -1), (9, -1), (10, -1), (11, -1), (12, 3)]

player1Prefs ::[IntPreference]
player1Prefs = [(-3, 2), (-5, 2), (5, 2), (3, 2)]

player2Prefs ::[IntPreference]
player2Prefs = [(5, 1), (3, 1)]


-- 
-- * Two-player pairings
--

-- | Play depth-limited minimax against a player who just plays the score.
scoreVsMiniMax :: [Player Improvise]
scoreVsMiniMax = [justTheScore, depthMiniMax]

-- | Play depth-limited minimax against itself.
miniVsMini :: [Player Improvise]
miniVsMini = [depthMiniMax, depthMiniMax]

-- | Play best 3 against best 3
bestVsBest :: [Player Improvise]
bestVsBest = [maximize, maximize]

-- | Play score against best 3
scoreVsBest :: [Player Improvise]
scoreVsBest = [justTheScore, maximize]

-- | Play two score-shifter against each other.
shiftVsShift :: [Player Improvise]
shiftVsShift = [shifter 4, shifter 4]

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

-- | A player who plays every note in her score shifted by the given number of 
-- half steps. 
shifter :: Int -> Player Improvise
shifter i = "Dr. Shifty" ::: shiftScore i

-- | A player who uses the best n strategy with n = 3
maximize :: Player Improvise
maximize  = "Missus Maxine" :::
    bestNLimited 3 4 (intervalPayoff [player1Prefs, player2Prefs])

-- | A player who plays randomly from the possible moves.
randy :: Player Improvise
randy = "Sir Randy" ::: randomly

--
-- * (players for testing)
--

-- | A player playing the minimax strategy.
testMinimax :: Player Improvise
testMinimax  = "Mr. Minimax" ::: minimax

