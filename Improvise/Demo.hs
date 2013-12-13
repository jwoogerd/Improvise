{-# LANGUAGE FlexibleContexts #-}

module Demo where

import Main
import Moves
import Game
import Conversions
import IO
import Players 
import Payoff

import Control.Monad.Trans (liftIO)
import Control.Monad (liftM)

import Euterpea hiding (Performance)
import Hagl

--
-- * These are some examples of Improvise game in action
--

-- | Just the score.
example1 = playImprovise 
                pay                          -- payoffs for this game
                (ByPlayer [mary, mary])      -- opening state of the game
                (limitByRange 2)             -- allowed range of deviation
                [justTheScore, justTheScore] -- players' strategies


-- | Random playing (within range).
example2 = playImprovise pay playMary 
                         (limitByRange 2) [randy, randy]


-- | This sounds good. 
example3 = playImprovise pay playMary
                         (limitByRange 2) [justTheScore, maximize pay]

-- | A journey.
example4 = do 
    start <- getFiles dontStop
    playImprovise pay start (limitByRange 2) 
                  [justTheScore, justTheScore]

-- | This actually sounds cool....
example5 = do 
    start <- getFiles dontStop
    playImprovise pay start (limitByRange 2) [justTheScore, maximize pay]

example6 = do 
    start <- getFiles dontStop
    playImprovise pay start (limitByRange 2) [maximize pay, justTheScore]

-- | The midi files.
dontStop :: [String]
dontStop = ["midi/DontStopMiddle.mid", "midi/DontStopBass.mid"]
                             
