{-# LANGUAGE FlexibleContexts #-}

module Demo where

import Main
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

-- | The midi files.
dontStop :: [String]
dontStop = ["midi/DontStopMiddle.mid", "midi/DontStopBass.mid"]

-- | Just the score.
example1 = evalGame 
    (Imp (intervalPayoff [player1Prefs, player2Prefs]) bothPlayingMary 2) 
    [justTheScore, justTheScore] (execute >> printSummary >> processMusic)

-- | Random playing (within range).
example2 = evalGame 
    (Imp (intervalPayoff [player1Prefs, player2Prefs]) bothPlayingMary 2) 
    [randy, randy] (execute >> printSummary >> processMusic)

-- | This sounds good. 
example3 = evalGame 
    (Imp (intervalPayoff [player1Prefs, player2Prefs]) bothPlayingMary 2) 
    [justTheScore, testBest3] (execute >> printSummary >> processMusic)

-- TODO: testBest3 v. testBest3 sounds exactly the same as above...we should
-- come up with another interesting example?

-- | A journey.
example5 = do 
    start <- getFiles dontStop
    evalGame 
        (Imp (intervalPayoff [player1Prefs, player2Prefs]) start 2) 
        [justTheScore, justTheScore] (execute >> printSummary >> processMusic)

-- | This actually sounds cool....
example6 = do 
    start <- getFiles dontStop
    evalGame (Imp (intervalPayoff [player1Prefs, player2Prefs]) start 8) 
             [testBest3, testBest3] (execute >> printSummary >> processMusic)


-- 
-- * Some helpers for the demo
--

getFiles :: [String] -> IO RealizationState
getFiles files = do 
    imported <- mapM importFile files 
    return (RS (map (musicToSS . fromEitherMidi) imported) [])

execute = step >>= maybe execute return
