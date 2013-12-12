module IO where

import Players
import Game
import Payoff
import Strategy
import Conversions

import Hagl
import Euterpea hiding (Player, Performance)

import Codec.Midi (Midi)
import System.Environment (getArgs)


-- ** Pick players

pickPlayer :: String -> IO (Player Improvise)
pickPlayer file = do 
    putStrLn ("Enter a name for the player of " ++ show file)
    name  <- readLn
    strat <- pickStrategy                    
    return (name ::: strat)


-- ** Pick a strategy for each player

allStrategies :: [(String, IO (Strategy () Improvise))]
allStrategies = [
    ("0. Play the move given by the score (don't improvise)",
        return myScore),
    ("1. Play the score shifted by the given number of half steps",
        do putStrLn ("Enter number of half steps! (warning -- if this is out "
                ++ "of your range, it will cause illegal moves!)");
           hs <- readLn;
           return $ shiftScore hs;)
    ]

pickStrategy :: IO (Strategy () Improvise)
pickStrategy = do 
    putStrLn "Pick a Strategy from the following list:"
    mapM (printStrLn . fst) allStrategies
    pick <- readLn
    snd $ allStrategies !! pick


-- ** Pick how payoffs are generated for each player

allPayoffs :: [String] -> [(String , IO (Performance -> Payoff))]
allPayoffs files = [
    ("0. Weight payoff based on interval preferences",
        do putStrLn ("Interval preferences are registered based on an integer "
                ++ "interval and a float weight to how much you \"like\" the "
                ++ "interval.\nNote that if you like all thirds, you should "
                ++ "enter -3 AND 3, to match intervals both above & below you")
           prefs <- mapM (pickPref True True) files
           return $ intervalPayoff prefs)
    ]

pickPref :: Bool -> Bool -> String -> IO ([IntPreference])
pickPref fst cont file = 
    if cont 
    then (if fst
          then putStrLn ("Choose preferences for " ++ show file)
          else return ()) >> do 
            putStrLn "Enter an integer representation of an interval."
            i <- readLn
            putStrLn "Enter a float weight"
            f <- readLn
            putStrLn "Do you want to enter another interval preference? (\"y\" for yes -- you must type quotes in your response!)"
            y <- readLn
            prefs <- pickPref False (y == "y") file
            return ((i,f):prefs)
    else return []

pickPayoff :: [String] -> IO (Performance -> Payoff)
pickPayoff files = do
     putStrLn "Pick a payoff function from the following list:"
     mapM (printStrLn . fst) (allPayoffs files)
     pick <- readLn
     snd $ allPayoffs files !! pick


getInteractive :: [String] 
    -> IO (Performance, [Player Improvise], Int, Performance -> Payoff)
getInteractive args = do
    imported <-  mapM importFile args
    players  <- mapM pickPlayer args
    putStrLn "How many half steps up/down should each player be able to improvise?"
    range <- readLn
    pay   <- pickPayoff args
    let start = extendPerformers $ ByPlayer $ map 
                (musicToPerformer . fromEitherMidi) imported
    return (start, players, range, pay)

fromEitherMidi :: Either String Midi -> Music Pitch
fromEitherMidi (Right m) = let (m2, _, _) = fromMidi m
                            in mMap fst m2

{-
configWithFiles :: [String] 
    -> IO (Performance, [Player Improvise], Int, Performance -> Payoff)
configWithFiles args = do
    imported <- mapM importFile args
    let start = extendPerformers $ ByPlayer $ map 
                (musicToPerformer . fromEitherMidi) imported
        pay     = intervalPayoff [prefs1, prefs2]
        players = [maximize pay, maximize pay]
        range   = 3
    return (start, players, range, pay)


config :: IO (Performance, [Player Improvise], Int, Performance -> Payoff)
config = let start   = ByPlayer [mary, mary]
             pay     = intervalPayoff [prefs1, prefs2]
             players = [maximize pay, maximize pay]
             range   = 3
          in return (start, players, range, pay)
-}

