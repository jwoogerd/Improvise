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

-- | Two players both with "Mary Had A Little Lamb" as their score.
bothPlayingMary = ByPlayer [mary, mary]

-- | Some sample player preferences.
prefs1, prefs2:: [IntPreference]
prefs1 = [(-3, 2), (-5, 2), (5, 2), (3, 2)]
prefs2 = [(5, 1), (3, 1)]

-- | Sample payoff generation scheme based on intervals and the player 
-- preferences above.
pay :: Performance -> Payoff
pay = intervalPayoff [prefs1, prefs2]

-- | Just the score.
example1 = playImprovise 
                pay                          -- payoffs for this game
                (ByPlayer [mary, mary])      -- opening state of the game
                (limitByRange 2)             -- allowed range of deviation
                [justTheScore, justTheScore] -- players' strategies


-- | Random playing (within range).
example2 = playImprovise pay bothPlayingMary 
                         (limitByRange 2) [randy, randy]


-- | This sounds good. 
example3 = playImprovise pay bothPlayingMary
                         (limitByRange 2) [justTheScore, maximize]

-- | A journey.
example4 = do 
    start <- getFiles dontStop
    playImprovise pay start (limitByRange 2) 
                  [justTheScore, justTheScore]

-- | This actually sounds cool....
example5 = do 
    start <- getFiles dontStop
    playImprovise pay start (limitByRange 2) [justTheScore, maximize]

example6 = do 
    start <- getFiles dontStop
    playImprovise pay start (limitByRange 2) [maximize, justTheScore]



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

playImprovise :: (Performance -> Payoff) 
              -> Performance 
              -> (Performance -> PlayerID -> [MusicMv])
              -> [Hagl.Player Improvise] 
              -> IO ()
playImprovise payoff start playable players  =
    evalGame (Imp payoff start playable) players 
             (execute >>
              liftIO (printStrLn "example ready...") >>
              liftIO getChar >>
              Demo.printGame >> processMusic) 
    where execute = step >>= maybe execute return

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
