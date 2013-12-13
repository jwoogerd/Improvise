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
import Hagl hiding (print)

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

-- | Play an Improvise game with the given payoff function, initial state,
-- move rules, and players.
playImprovise :: (Performance -> Payoff) -> 
                 Performance -> 
                 (Performance -> PlayerID -> [MusicMv]) -> 
                 [Hagl.Player Improvise] -> 
                 IO ()
playImprovise payoff start playable players  =
    evalGame (Imp start payoff playable) players 
             (execute >>
              liftIO (printStrLn "example ready...") >>
              liftIO getChar >>
              printGame >> processMusic) 
    where execute = step >>= maybe execute return


main = playImprovise pay 
                     (ByPlayer [mary, mary]) 
                     (limitByRange 2) 
                     [justTheScore, maximize pay] 


--
-- * Convert between MIDI and Performance
--

-- | Import a list of midi files from disk and construct an initial state
-- Performance from them.
getFiles :: [String] -> IO Performance
getFiles files = do 
    imported <- mapM importFile files 
    return $ extendPerformers 
             (ByPlayer (map (musicToPerformer . fromEitherMidi) imported))

-- | Write a performance out to disk in midi format.
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
          then liftM (getPerformance . fst . forGame 1) summaries
          else liftM treeState location 
    let mus = performanceToMusic performance
    liftIO $ Euterpea.play mus
   -- liftIO $ exportMusic mus
    return ()


-- 
-- * Printing
--

-- | Print summary of the last game.
printGame :: (GameM m Improvise, Show (Move Improvise)) => m ()
printGame = do n <- numCompleted
               (mss,pay) <- liftM (forGame n) summaries
               ps <- players
               printStrLn $ "Summary of Game "++show n++":"
               printStrLn "Players: "
               printStrLn $ show $ map printPlayer (everyPlayer ps)
               liftIO (printMoves mss)
               printMaybePayoff pay
    where printPlayer p = take 18 $ show p ++ repeat ' '

-- | Pretty print moves for two-player games in two columns.
printMoves :: ByPlayer (ByTurn (Move Improvise))-> IO ()
printMoves mss = let mvs = map everyTurn (everyPlayer mss)
                     build n = if n == 0 then [] else (n-1):build (n-1)
                     base = build (length (head mvs))
                     getAllNth n = map (!! n) mvs
                 in  mapM_ (print . getAllNth) base 

-- | String representation of a move summary.
showMoveSummary :: (Game g, Show (Move g)) => ByPlayer (Hagl.Player g) -> 
                                              MoveSummary (Move g) -> 
                                              String
showMoveSummary ps mss = 
    (unlines . map row) 
    (zip (everyPlayer ps) (map everyTurn (everyPlayer mss)))
        where row (p,ms) = "  " ++ show p ++ " moves: " 
                           ++ showSeq (reverse (map show ms))



{-
parse ["-n"] = config
parse ("-fc":args) = configWithFiles args
parse  args  = getInteractive args

main = getArgs >>= parse >>= (\(start, players, range, pay) ->
          evalGame (Imp pay (extendPerformers start) range)
                  players (run >> printSummary))
                where run = step >>= maybe run (\p -> printGame >> processMusic >> return p) 
              
-}


