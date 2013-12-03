{-# LANGUAGE FlexibleContexts #-}
module Main where

import Game
import Strategy
import Conversions
import State
import Players
import Euterpea
import Hagl
import Codec.Midi
import Control.Monad.Trans (liftIO)
import Control.Monad (liftM,liftM2,unless)
import Control.Monad.State


main = evalGame (Imp (intervalPayoff [player1Prefs,player2Prefs]) start) guessPlayers (run >> printSummary)
   where run = step >>= maybe run (\p -> printGame >> playMusic >>return p)

-- Printing
printGame :: GameM m Improvise => m ()
printGame = gameState >>= liftIO . putStrLn . show

-- Music generation
playMusic :: (GameM m Improvise, Show (Move Improvise)) => m ()
playMusic = do
    (mss, _) <- liftM (forGame 1) summaries
    liftIO $ Euterpea.play $ rsToMusic (getRS mss)
    return ()

-- | String representation of a move summary.
showMoveSummary :: (Game g, Show (Move g)) =>
  ByPlayer (Hagl.Player g) -> MoveSummary (Move g) -> String
showMoveSummary ps mss = (unlines . map row)
                         (zip (everyPlayer ps) (map everyTurn (everyPlayer mss)))
  where row (p,ms) = "  " ++ show p ++ " moves: " ++ showSeq (reverse (map show ms))


-- starting to import music
testFile :: IO (Either String Codec.Midi.Midi)
testFile = importFile "test.MID"

fromEitherMidi :: Either String Codec.Midi.Midi
     -> Music Pitch
fromEitherMidi (Right m) = 
    let (m2, _, _) = fromMidi m
    in mMap fst m2

iomus = liftM fromEitherMidi testFile

fromio = iomus >>= Euterpea.play

--main = evalGame Improvise guessPlayers (run >> printSummary)
--   where run = step >>= maybe run (\p -> printGame >> playMusic >>return p)
