{-# LANGUAGE FlexibleContexts #-}
module Main where

import Game
import Strategy
import Conversions
import State
import Payoff
import Players
import Euterpea hiding (Performance)
import Hagl
import Codec.Midi (Midi)
import Control.Monad.Trans (liftIO)
import Control.Monad (liftM,liftM2,unless)
import System.Environment (getArgs)
import IO

main =
    evalGame (Imp (intervalPayoff [player1Prefs,player2Prefs]) (ByPlayer [mary, mary]) 2)
             [maximize, maximize] (run >> printSummary)
        where run = step >>= maybe run 
                                   (\p -> printGame >> processMusic >> return p)

{-
parse ["-n"] = config
parse ("-fc":args) = configWithFiles args
parse  args  = getInteractive args

main = getArgs >>= parse >>= (\(start, players, range, pay) ->
          evalGame (Imp pay (extendSSs start) range)
                  players (run >> printSummary))
                where run = step >>= maybe run (\p -> printGame >> processMusic >> return p) 
              
-}
exportMusic :: Music Pitch -> IO ()
exportMusic mus = do
    putStrLn "Where should we export your music? (enter with quotation marks)"
    fname <- readLn
    exportFile fname (testMidi mus)




-- | Music generation
processMusic :: (GameM m Improvise, Show (Move Improvise)) => m ()
processMusic = do
    b <- isNewGame;
    performance <- if b
          then liftM (getPerformance . fst . (forGame 1)) summaries
          else liftM treeState location 
    let mus = performanceToMusic performance
    liftIO $ Euterpea.play mus
--    liftIO $ exportMusic mus
    return ()

-- 
-- * Printing
--

-- | Print an Improvise game.
printGame :: GameM m Improvise => m ()
printGame = gameState >>= liftIO . putStrLn . show

-- | String representation of a move summary.
showMoveSummary :: (Game g, Show (Move g)) =>
  ByPlayer (Hagl.Player g) -> MoveSummary (Move g) -> String
showMoveSummary ps mss = (unlines . map row)
                         (zip (everyPlayer ps) (map everyTurn (everyPlayer mss)))
  where row (p,ms) = "  " ++ show p ++ " moves: " ++ showSeq (reverse (map show ms))


