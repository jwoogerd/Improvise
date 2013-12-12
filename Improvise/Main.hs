{-# LANGUAGE FlexibleContexts #-}

module Main where

import Game
import Strategy
import Conversions
import Payoff
import Players
import Moves 
import IO

import Euterpea hiding (Performance)
import Hagl

import Codec.Midi (Midi)
import Control.Monad.Trans (liftIO)
import Control.Monad (liftM,liftM2,unless)
import System.Environment (getArgs)


-- | The opening notes to "Mary Had A Little Lamb".
mary :: Performer
mary = Performer [] [Begin (A,4), Extend (A, 4), Begin (G,4), Extend (G, 4),
                 Begin (F, 4), Extend (F, 4), Begin (G, 4), Extend (G, 4),
                 Begin (A, 4), Extend (A, 4), Begin (A, 4), Extend (A, 4),
                 Begin (A, 4), Extend (A, 4), Extend (A, 4), Extend (A, 4)]


-- | An example initial game state.
playMary = ByPlayer [mary, mary]

-- | Some sample player preferences.
prefs1, prefs2:: [IntPreference]
prefs1 = [(-3, 2), (-5, 2), (5, 2), (3, 2)]
prefs2 = [(5, 1), (3, 1)]

-- | Sample payoff generation scheme based on intervals and the player 
-- preferences above.
pay :: Performance -> Payoff
pay = intervalPayoff [prefs1, prefs2]


main = evalGame (Imp playMary pay (limitByRange 2))
                [maximize pay, maximize pay] 
                (run >> printSummary)
        where run = 
                step >>= maybe run (\p -> printGame >> processMusic >> return p)

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


