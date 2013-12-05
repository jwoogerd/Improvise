{-# LANGUAGE FlexibleContexts #-}

module Demo where

import Main
import Moves
import Game
import State
import Conversions
import IO
import Players 
import Payoff

import Control.Monad (liftM)

import Euterpea
import Hagl

--
-- * These are some examples of Improvise game in action
--

-- | Two players both with "Mary Had A Little Lamb" as their score.
bothPlayingMary = RS [mary, mary] []

-- | Some sample player preferences.
prefs1, prefs2 :: [IntPreference]
prefs1 = [(-3, 2), (-5, 2), (5, 2), (3, 2)]
prefs2 = [(5, 1), (3, 1)]

-- | Just the score.
example1 = playImprovise 
                [prefs1, prefs2]             -- players' interval preferences
                intervalPayoff               -- interval-based payoff function 
                (RS [mary, mary] [])         -- opening state of the game
                2                            -- allowed range of deviation
                [justTheScore, justTheScore] -- players' strategies


-- | Random playing (within range).
example2 = playImprovise [prefs1, prefs2] intervalPayoff bothPlayingMary 2 
                         [randy, randy]


-- | This sounds good. 
example3 = playImprovise [prefs1, prefs2] intervalPayoff bothPlayingMary 2
                         [maximize, maximize]


-- | A journey.
example4 = do 
    start <- getFiles dontStop
    playImprovise [prefs1, prefs2] intervalPayoff start 2 
                  [justTheScore, justTheScore]


-- | This actually sounds cool....
example5 = do 
    start <- getFiles dontStop
    playImprovise [prefs1, prefs2] intervalPayoff start 2 [maximize, maximize]

-- | Randy playing Don't Stop
example6 = do
    start <- getFiles dontStop
    playImprovise [prefs1, prefs2] intervalPayoff start 2 [randy, randy]

-- 
-- * Some helpers for the demo
--


-- | The midi files.
dontStop :: [String]
dontStop = ["midi/DontStopMiddle.mid", "midi/DontStopBass.mid"]

getFiles :: [String] -> IO RealizationState
getFiles files = do 
    imported <- mapM importFile files 
    return (RS (map (musicToSS . fromEitherMidi) imported) [])

playImprovise :: [[IntPreference]] -> ([[IntPreference]] -> RealizationState 
    -> Payoff) -> RealizationState -> Range -> [Hagl.Player Improvise] -> IO ()
playImprovise prefs payoff start range players  = 
    evalGame (Imp (payoff prefs) start range) players 
             (execute >> printSummary >> processMusic) 

execute = step >>= maybe execute return
