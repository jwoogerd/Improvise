{-# LANGUAGE FlexibleContexts #-}

module Strategy where

import Game
import State
import Hagl
import Euterpea (absPitch, Pitch)
import Control.Monad (liftM)
import Data.Function (on)
import Data.List     (maximumBy, sortBy)

--
-- * Payoff generation
--

type Interval = Int
type IntPreference = (Interval, Float)

interval :: Pitch -> Pitch -> Int
interval p1 p2 = absPitch p2 - absPitch p1

intPref :: [IntPreference] -> Int -> Float
intPref prefs i = foldr f 0 prefs
    where f (interval, pay) acc = if i == interval then pay + acc else acc

rmoveInterval :: RMove -> RMove -> Maybe Interval
rmoveInterval (Begin p1)  (Begin p2)  = Just $ interval p1 p2
rmoveInterval (Begin p1)  (Extend p2) = Just $ interval p1 p2
rmoveInterval (Extend p1) (Begin p2)  = Just $ interval p1 p2
rmoveInterval (Extend p1) (Extend p2) = Just $ interval p1 p2
rmoveInterval _           _           = Nothing

onePlayerPay :: [RMove] -> [[RMove]] -> [IntPreference] -> Float
onePlayerPay [] _ _ = 0
onePlayerPay _ [] _ = 0
onePlayerPay (me:rs) others ps = foldr f 0 others + onePlayerPay rs (map tail others) ps
    where f (m:ms) acc = case rmoveInterval me m of
                            Nothing -> acc
                            Just a  -> acc + intPref ps a

intervalPayoff :: [[IntPreference]] -> RealizationState -> Payoff
intervalPayoff prefs rs = ByPlayer $ p [] (scores rs) prefs
    where p _      []         _     = []
          p before (me:after) (myPrefs:ps) = 
              onePlayerPay (realization me) 
              (map realization (before ++ after)) myPrefs: 
              p (me:before) after ps

--
-- * Strategies
--

-- | Strategy for always playing the move given by the score; i.e. never
-- improvising.
myScore :: DiscreteGame Improvise => Strategy () Improvise
myScore = liftM myScoreAlg location
    where myScoreAlg (Discrete (RS scores accum, _) _) = 
            let SS _ future = scores !! length accum
            in head future


-- | Strategy that always chooses the first available move
firstOpt :: Discrete s mv -> mv
firstOpt (Discrete (_,Decision me) edges) = fst $ head edges

-- | Minimax strategy.  Computes the best move for the current player with 
-- depth limit.  Uses the given depth limit and state-based payoff function 
-- to call when limit reached.
minimaxLimited :: DiscreteGame g => 
    Integer -> (State g -> Payoff) -> Strategy () g
minimaxLimited i p = liftM (minimaxABLimited i p) location
                         

-- | Minimax depth limited algorithm
minimaxABLimited :: Integer -> (s -> Payoff) -> Discrete s mv  -> mv
minimaxABLimited depth pay (Discrete (_,Decision me) edges) = 
    fst $ maximumBy (compare `on` snd) 
        [(m, mm t depth (-inf) inf) | (m,t) <- edges]
    where inf = 1/0 :: Float
          mm (Discrete ( _, Payoff   vs) _    ) d a b = forPlayer me vs  
          mm (Discrete ( s, Decision  p) edges) d a b = 
            if d == 0 
            then forPlayer me (pay s)
            else
                if me == p
                then foldr (\t a' -> if a' < b
                                     then max (mm t (d - 1) a' b) a'
                                     else a')
                            a
                            (map snd edges)
                else foldr (\t b' -> if a < b'
                                     then min (mm t (d - 1) a b') b'
                                     else b')
                            b
                            (map snd edges)
minimaxABLimited _ _ _ = 
    error "minimaxAlg: root of game tree is not a decision!"

{-
bestHalfLimited :: Integer -> (s -> Payoff) -> Discrete s mv -> mv
bestHalfLimited depth pay (Discrete (_,Decision me) edges) =
    fst $ maximumBy (compare `on` snd)
        [(m, bestHalf me t depth) | (m,t) <- edges]
    where sortFunc :: PlayerID -> Discrete s mv  -> Float
          sortFunc p (Discrete ( _, Payoff vs) _) = forPlayer p vs
          sortFunc p (Discrete ( s, _        ) _) = forPlayer p $ pay s
          bestHalf :: PlayerID -> Discrete s mv -> Integer -> [Float]
          bestHalf who (Discrete ( _, Payoff   vs) _    ) d = [forPlayer who vs]
          bestHalf who (Discrete ( s, Decision  p) edges) d = 
            if d == 0
            then [forPlayer who (pay s)]
            else let sf2 :: Edge s mv -> Float
                     sf2 = (sortFunc who) . edgeDest
                     paths :: [Discrete s mv]
                     paths = sortBy (compare `on` sf2) edges
                  in map (\tree -> bestHalf p tree (d - 1)) $ take (div (length edges) 2) paths         
                 -} 
