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

-- | bestN Strategy.  --TODO LET ANDREW WRITE THIS EXPLANATION
bestNLimited :: DiscreteGame g => Int -> Integer -> (State g -> Payoff) -> Strategy () g
bestNLimited n depth pay = liftM (bestNLimitedAlg n depth pay) location


bestNLimitedAlg :: Int -> Integer -> (s -> Payoff) -> Discrete s mv -> mv
bestNLimitedAlg n depth pay (Discrete (_,Decision me) edges) = 
    let sortFunc p (Discrete ( _, Payoff vs) _) = forPlayer p vs
        sortFunc p (Discrete ( s, _        ) _) = forPlayer p $ pay s
        bestN who (Discrete ( _, Payoff   vs) _    ) d = forPlayer who vs
        bestN who (Discrete ( s, Decision  p) edges) d =
            if d == 0
            then forPlayer who (pay s)
            else let paths = sortBy (compare `on` (sortFunc who)) (map edgeDest edges)
                  in maximumBy compare $ map (\tree -> bestN p tree (d - 1)) (take n paths)
     in fst $ maximumBy (compare `on` snd)
            [(m, bestN me t depth) | (m,t) <- edges]
bestNLimitedAlg _ _ _ _ = 
    error "bestNLimitedAlg: root of game tree is not a decision!"

