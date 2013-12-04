{-# LANGUAGE FlexibleContexts #-}

module Strategy where

import Game
import State
import Hagl
import Euterpea (absPitch, Pitch, trans)
import Control.Monad (liftM)
import Data.Function (on)
import Data.List     (maximumBy, sortBy)
import Debug.Trace
--
-- * Strategies
--
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


-- | Strategy for always playing the move given by the score; i.e. never
-- improvising.
myScore :: DiscreteGame Improvise => Strategy () Improvise
myScore = liftM myScoreAlg location
    where myScoreAlg (Discrete (RS scores accum, _) _) = 
            let SS _ future = scores !! length accum
            in head future

-- | Strategy for playing the score shifted by the given number of half steps.
shiftScore :: DiscreteGame Improvise => Int -> Strategy () Improvise
shiftScore i = liftM shiftAlg location
    where shiftAlg (Discrete (RS scores accum, _) _) =
                let SS _ future = scores !! length accum
                in shift i $ head future
          shift i (Begin p)  = Begin (trans i p)
          shift i (Extend p) = Extend (trans i p)
          shift _ _          = State.Rest       

-- | Strategy that always chooses the first available move
firstOpt :: Discrete s mv -> mv
firstOpt (Discrete (_, Decision me) edges) = fst $ head edges

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

