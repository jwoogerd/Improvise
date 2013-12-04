{-# LANGUAGE FlexibleContexts #-}

module Payoff where

import Game
import State
import Hagl
import Euterpea (absPitch, Pitch)
import Control.Monad (liftM)
import Data.Function (on)
import Data.List     (maximumBy, sortBy)

{- 

This module contains code for various payoff generation schemes based on some
notion of each players' musical aesthetic preferences.

-}
allPayoffs :: [String] -> [(String , IO (RealizationState -> Payoff))]
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

pickPayoff :: [String] -> IO (RealizationState -> Payoff)
pickPayoff files = do
     putStrLn "Pick a payoff function from the following list:"
     mapM (printStrLn . fst) (allPayoffs files)
     pick <- readLn
     snd $ (allPayoffs files) !! pick

    

--
-- * Interval-based payoff generation
--

-- ** In this scheme, payoffs are generated based on each players'
-- preference for certain musical intervals.

-- | A musical interval, an integer difference between two pitches.
type Interval = Int

-- | An interval preference is represented as the interval and the payoff the
-- player would recieve by playing it. Positive payoffs denote favorable
-- intervals, negative payoffs signify undesirable ones.
type IntPreference = (Interval, Float)

-- | Compute the integer difference between two pitches.
interval :: Pitch -> Pitch -> Int
interval p1 p2 = absPitch p2 - absPitch p1

-- | Sum the total payoff for a player, given his preferences and an interval.
intPref :: [IntPreference] -> Int -> Float
intPref prefs i = foldr f 0 prefs
    where f (interval, pay) acc = if i == interval then pay + acc else acc

-- | Find the interval between two moves, if one exists.
rmoveInterval :: RMove -> RMove -> Maybe Interval
rmoveInterval (Begin p1)  (Begin p2)  = Just $ interval p1 p2
rmoveInterval (Begin p1)  (Extend p2) = Just $ interval p1 p2
rmoveInterval (Extend p1) (Begin p2)  = Just $ interval p1 p2
rmoveInterval (Extend p1) (Extend p2) = Just $ interval p1 p2
rmoveInterval _           _           = Nothing

-- | Calculate the payoff for a single player, given his preferences.
onePlayerPay :: [RMove] -> [[RMove]] -> [IntPreference] -> Float
onePlayerPay [] _ _ = 0
onePlayerPay _ [] _ = 0
onePlayerPay (me:rs) others ps = 
    foldr f 0 others + onePlayerPay rs (map tail others) ps
    where f (m:ms) acc = case rmoveInterval me m of
                            Nothing -> acc
                            Just a  -> acc + intPref ps a

-- | Generate a payoff matrix from a list of the players' preferences and their
-- respective scores, which are given by the game state.
intervalPayoff :: [[IntPreference]] -> RealizationState -> Payoff
intervalPayoff prefs rs = ByPlayer $ p [] (scores rs) prefs
    where p _      []         _            = []
          p _      _          []           = []
          p before (me:after) (myPrefs:ps) = 
            onePlayerPay (realization me) 
                         (map realization (before ++ after)) myPrefs: 
                            p (me:before) after ps


--
-- * Counterpoint-based payoff generation
--

-- TODO: this. (Andrew?)
