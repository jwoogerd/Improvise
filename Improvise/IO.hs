module IO where

import Players
import Game
import Payoff
import Conversions

import Hagl
import Euterpea hiding (Player, Performance)
import Codec.Midi (Midi)
import System.Environment (getArgs)



getInteractive :: [String] -> IO (Performance, [Player Improvise], Int, Performance -> Payoff)
getInteractive args = do
    imported <-  mapM importFile args
    players <- mapM pickPlayer args
    putStrLn "How many half steps up/down should each player be able to improvise?"
    range <- readLn
    pay <- pickPayoff args
    let start = extendPerformers $ ByPlayer $ map (musicToPerformer . fromEitherMidi) imported
    return (start, players, range, pay)

fromEitherMidi :: Either String Midi -> Music Pitch
fromEitherMidi (Right m) = let (m2, _, _) = fromMidi m
                            in mMap fst m2


configWithFiles :: [String] -> IO (Performance, [Player Improvise], Int, Performance -> Payoff)
configWithFiles args = do
    imported <- mapM importFile args
    let start = extendPerformers $ ByPlayer $ map (musicToPerformer . fromEitherMidi) imported
        players = [maximize,maximize]
        range   = 3
        pay     = intervalPayoff [player1Prefs,player2Prefs]
    return (start, players, range, pay)

config :: IO (Performance, [Player Improvise], Int, Performance -> Payoff)
config = let start   = ByPlayer [mary, justCNotes]
             players = [maximize,maximize]
             range   = 3
             pay     = intervalPayoff [player1Prefs,player2Prefs]
          in return (start, players, range, pay)


