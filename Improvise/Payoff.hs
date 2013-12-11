{-# LANGUAGE FlexibleContexts #-}

module Payoff where

import Game
import Hagl
import Euterpea (absPitch, Pitch)

import Data.Maybe (fromMaybe)
import Data.List  (lookup)

{- 

This module contains code for various payoff generation schemes based on some
notion of each players' musical aesthetic preferences.

-}

--
-- * Interval-based payoff generation
--

-- ** In this scheme, payoffs are generated based on each players'
-- preference for certain musical intervals.

-- | A musical interval, an integer difference between two pitches.
type Interval = Int

-- | An interval preference is an association list of musical intervals and 
-- the payoff the player would receive by playing it. Positive payoffs denote 
-- favorable intervals, negative payoffs signify undesirable ones.
type IntPreference = (Interval, Float)

-- | Compute the integer difference between two pitches, from p1 to p2. Full 
-- octaves spanned are ignored. Returns -12 to 12.
interval :: Pitch -> Pitch -> Int
interval p1 p2 =
  let pitch1 = absPitch p1
      pitch2 = absPitch p2
  in if (pitch2 `mod` 12) >= (pitch1 `mod` 12)
     then (pitch2 `mod` 12) - (pitch1 `mod` 12)
     else (pitch2 `mod` 24) - (pitch1 `mod` 12)

-- | Compute the integer difference between two pitches, from p1 to p2. Full 
-- octaves spanned are included.
interval8ve :: Pitch -> Pitch -> Int
interval8ve p1 p2 = absPitch p2 - absPitch p1

-- | Find the interval between two moves, if one exists.
moveInterval :: MusicMv -> MusicMv -> Maybe Interval
moveInterval (Begin p1)  (Begin p2)  = Just $ interval p1 p2
moveInterval (Begin p1)  (Extend p2) = Just $ interval p1 p2
moveInterval (Extend p1) (Begin p2)  = Just $ interval p1 p2
moveInterval (Extend p1) (Extend p2) = Just $ interval p1 p2
moveInterval _           _           = Nothing

-- | Calculate the payoff for a single player, given the moves of all the 
-- other players and his preferences.
myPayoff :: [MusicMv] -> [[MusicMv]] -> [IntPreference] -> Float
myPayoff []              _      _     = 0
myPayoff  _              []     _     = 0
myPayoff (myMove:myPast) others prefs = 
    foldr findIntervals 0 others + myPayoff myPast (map tail others) prefs
    where findIntervals (hisMove:ms) pay = 
            case moveInterval myMove hisMove of
                    Just int -> pay + fromMaybe 0 (lookup int prefs)
                    Nothing  -> pay 

-- | Generate a payoff matrix from a list of the players' preferences and their
-- respective scores, which are given by the game state.
intervalPayoff :: [[IntPreference]] -> Performance -> Payoff
intervalPayoff prefs performance = 
    ByPlayer $ intervalPay [] (everyPlayer performance) prefs
    where intervalPay _    []                 _            = []
          intervalPay _     _                []            = []
          intervalPay done (thisPlayer:next) (herPrefs:ps) = 
            let herMoves       = realization thisPlayer
                othersMoves    = map realization (done ++ next)
                thisPlayersPay = myPayoff herMoves othersMoves herPrefs
            in thisPlayersPay : intervalPay (thisPlayer:done) next ps

