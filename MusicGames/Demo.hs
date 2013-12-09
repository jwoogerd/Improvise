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

import Control.Monad.Trans (liftIO)
import Control.Monad (liftM)

import Euterpea hiding (Performance)
import Hagl

--
-- * These are some examples of Improvise game in action
--

-- | Two players both with "Mary Had A Little Lamb" as their score.
bothPlayingMary = ByPlayer [mary, mary]

-- | Some sample player preferences.
prefs1, prefs2:: [IntPreference]
prefs1 = [(-3, 2), (-5, 2), (5, 2), (3, 2)]
prefs2 = [(5, 1), (3, 1)]


-- | Just the score.
example1 = playImprovise 
                [prefs1, prefs2]             -- players' interval preferences
                intervalPayoff               -- interval-based payoffs 
                (ByPlayer [mary, mary])      -- opening state of the game
                (limitByRange 2)             -- allowed range of deviation
                [justTheScore, justTheScore] -- players' strategies


-- | Random playing (within range).
example2 = playImprovise [prefs1, prefs2] intervalPayoff bothPlayingMary 
                         (limitByRange 2) [randy, randy]


-- | This sounds good. 
example3 = playImprovise [prefs1, prefs2] intervalPayoff bothPlayingMary
                         (limitByRange 2) [justTheScore, maximize]


-- | A journey.
example4 = do 
    start <- getFiles dontStop
    playImprovise [prefs1, prefs2] intervalPayoff start (limitByRange 2)
                  [justTheScore, justTheScore]

-- | This actually sounds cool....
example5 = do 
    start <- getFiles dontStop
    playImprovise [prefs1, prefs2] intervalPayoff start (limitByRange 2)
                  [justTheScore, maximize]

example6 = do 
    start <- getFiles dontStop
    playImprovise [prefs1, prefs2] intervalPayoff start (limitByRange 2) 
                  [maximize, justTheScore]

-- 
-- * Some helpers for the demo
--


-- | The midi files.
dontStop :: [String]
dontStop = ["midi/DontStopMiddle.mid", "midi/DontStopBass.mid"]

getFiles :: [String] -> IO Performance
getFiles files = do 
    imported <- mapM importFile files 
    return $ extendPerformers 
             (ByPlayer (map (musicToPerformer . fromEitherMidi) imported))

playImprovise :: [[IntPreference]] 
              -> ([[IntPreference]] 
              -> Performance -> Payoff) 
              -> Performance 
              -> (PlayerID -> Performance -> [MusicMv])
              -> [Hagl.Player Improvise] 
              -> IO ()
playImprovise prefs payoff start range players  =
    evalGame (Imp (payoff prefs) start range) players 
             (execute >>
              liftIO (printStrLn "example ready...") >>
              liftIO getChar >>
              Demo.printGame >> processMusic) 

execute = step >>= maybe execute return


prefs3 = [(5, 1), (3, 1), (8, -3), (-8, -3)]
prefs12 = [(0, 2), (1, -2), (-1, -2), (2, -1), (-2, -2),
                   (3,  0), (-3,  0), (4,  3), (-4,  3),
                   (5,  0), (-5,  0), (6, -3), (-6, -3),
                   (7,  4), (-7,  4), (8, -1), (-8, -1),
                   (9,  0), (-9,  0), (10,-1), (-10,-1),
                   (11,-2), (-11,-2), (12, 2), (-12,-2)]

-- | Print summary of the last game.
printGame :: (GameM m Improvise, Show (Move Improvise)) => m ()
printGame = do n <- numCompleted
               (mss,pay) <- liftM (forGame n) summaries
               ps <- players
               printStrLn $ "Summary of Game "++show n++":"
               printStrLn $ "Players: "
               printStrLn $ show $ map printPlayer (everyPlayer ps)
               liftIO (printMoves mss)
               printMaybePayoff pay

printPlayer p = take 18 $ show p ++ repeat ' '

printMoves :: ByPlayer (ByTurn (Move Improvise))-> IO ()
printMoves mss = let mvs = map everyTurn (everyPlayer mss)
                     build n = if (n==0) then [] else (n-1):(build (n-1))
                     base = build (length (head mvs))
                     getAllNth n = map (flip (!!) n) mvs
                 in  mapM_ (putStrLn . show . getAllNth) base                                
