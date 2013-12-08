{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Game where

import Euterpea
import Hagl
import State
import Moves
import Debug.Trace

--
-- * Game representation 
--

-- ** These functions are common to all Improvise game executions.

-- | Whose turn it is.

-- | True when the game is over. 
end :: RealizationState -> Bool
end (RS scores accumulating) = foldr ((&&) . null . future) True scores
--null accumulating && null (future (head scores))

-- | Get a list of the available moves given the allowed range and score.
playable :: Range -> RealizationState -> PlayerID -> [MusicMv]
playable r rs p = availableMoves r $ scores rs !! (p - 1)

-- | Players make their moves
registerMoves :: RealizationState -> [MusicMv] -> RealizationState -- front most move should go in the last score
registerMoves rs mvs 
    | length mvs == length (scores rs) = RS (zipWith register (scores rs) (reverse mvs)) []
    | otherwise = error "tried to register different number of moves than existing players"
        where register ss mv = SS (mv: realization ss) (tail (future ss))

-- | The Improvise data type wraps information unique to a particular game 
-- execution: a description of the players' musical aesthetic preferences (i.e. 
-- a function to generate payoffs); the game state (the scores); and the range 
-- of allowed deviation from the score for this game execution.
data Improvise = Imp { payoff :: RealizationState -> Payoff
                     , state  :: RealizationState
                     , range  :: Range}
    

-- | Game instance
instance Game Improvise where
  type TreeType Improvise = Discrete
  type Move  Improvise = MusicMv
  type State Improvise = RealizationState
  
  gameTree (Imp payoff state range) = simStateTreeD end (playable range) registerMoves payoff (length (scores state)) state
    --stateTreeD who end (playable range) registerMove payoff state

-- | Build a discrete game tree for a state-based game.
simStateTreeD ::
        (s -> Bool)     -- ^ Is the game over?
     -> (s -> PlayerID -> [mv])     -- ^ Available moves.
     -> (s -> [mv] -> s)  -- ^ Execute the moves and return the new state.
     -> (s -> Payoff)   -- ^ Payoff for this (final) state.
     -> Int             -- ^ number of players
     -> s               -- ^ The current state.
     -> Discrete s mv
simStateTreeD end moves exec pay np = tree 1 []
  where tree p ms s | end s        = Discrete (s, Payoff (pay s)) []
                    | p < np       = Discrete (s, Decision p) [(m, tree (p+1) (m:ms) s) | m <- moves s p]
                    | p == np      = Discrete (s, Decision p) [(m, tree 1 [] (exec s (m:ms))) | m <- moves s p]
                    | otherwise    = error "what happen"


