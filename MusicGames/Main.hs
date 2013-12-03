{-# LANGUAGE FlexibleContexts #-}
module Main where

import Game
import Strategy
import Conversions
import State
import Payoff
import Players
import Euterpea
import Hagl
import Codec.Midi (Midi)
import Control.Monad.Trans (liftIO)
import Control.Monad (liftM,liftM2,unless)
import Control.Monad.State
import System.Environment (getArgs)

{-
main =
    evalGame (Imp (intervalPayoff [player1Prefs,player2Prefs]) start 2) 
             scoreVsBest (run >> printSummary)
        where run = step >>= maybe run 
                                   (\p -> printGame >> playMusic >> return p)
                                   -}
main = do args <- getArgs;
          imported <-  mapM importFile args;
          Euterpea.play $ foldr (:+:) (Prim (Euterpea.Rest 0)) (map fromEitherMidi imported) 


fromEitherMidi :: Either String Midi -> Music Pitch
fromEitherMidi (Right m) = let (m2, _, _) = fromMidi m
                            in mMap fst m2



-- | Music generation
playMusic :: (GameM m Improvise, Show (Move Improvise)) => m ()
playMusic = do
    (mss, _) <- liftM (forGame 1) summaries
    liftIO $ Euterpea.play $ rsToMusic (getRS mss)
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


