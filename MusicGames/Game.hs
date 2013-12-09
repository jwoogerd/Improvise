{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Game where

import Euterpea (Pitch)
import Hagl
import State
import Moves

--
-- * Game representation 
--

-- | The Improvise data type wraps information unique to a particular game 
-- execution: a description of the players' musical aesthetic preferences (i.e. 
-- a function to generate payoffs); the game state (the scores); and a function
-- for determining legal moves from the given game state. 
data Improvise = Imp { payoff   :: Performance -> Payoff
                     , state    :: Performance
                     , playable :: Performance -> PlayerID -> [MusicMv]}
 

-- ** These functions are common to all Improvise game executions.

-- | True when the game is over. 
end :: Performance -> Bool
end performance = foldr ((&&) . null . future) True (everyPlayer performance)

-- | Players make their moves
-- front most move should go in the last score
registerMoves :: Performance -> [MusicMv] -> Performance 
registerMoves performance mvs 
    | length mvs == length (everyPlayer performance) = 
        ByPlayer (zipWith register (everyPlayer performance) (reverse mvs))
    | otherwise = 
        error "Cannot register different number of moves than existing players"
        where register ss mv = Performer (mv: realization ss) (tail (future ss))
   

-- | Game instance
instance Game Improvise where
  type TreeType Improvise = Discrete
  type Move  Improvise = MusicMv
  type State Improvise = Performance
  
  gameTree (Imp payoff state playable) = simStateTreeD 
    end playable registerMoves payoff (length (everyPlayer state)) state


--
-- * Game tree
--

-- | Build a discrete game tree for a state-based game.
simStateTreeD ::
        (s -> Bool)             -- ^ Is the game over?
     -> (s -> PlayerID -> [mv]) -- ^ Available moves.
     -> (s -> [mv] -> s)        -- ^ Execute the moves and return the new state.
     -> (s -> Payoff)           -- ^ Payoff for this (final) state.
     -> Int                     -- ^ number of players
     -> s                       -- ^ The current state.
     -> Discrete s mv
simStateTreeD end moves exec pay np = tree 1 []
  where tree p ms s 
            | end s     = Discrete (s, Payoff (pay s)) []
            | p < np    = Discrete (s, Decision p) 
                               [(m, tree (p+1) (m:ms) s) | m <- moves s p]
            | p == np   = Discrete (s, Decision p) 
                               [(m, tree 1 [] (exec s (m:ms))) | m <- moves s p]
            | otherwise = error "what happen"

