{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Game where

import Euterpea
import Hagl
import State
import Moves

--
-- * Game representation 
--

-- ** These functions are common to all Improvise game executions.

-- | Whose turn it is.
who :: RealizationState -> PlayerID
who rs = length (accumulating rs) + 1

-- | True when the game is over. 
end :: RealizationState -> Bool
end (RS scores accumulating) = null accumulating && null (future (head scores))

-- | Get a list of the available moves given the allowed range and score.
playable :: Range -> RealizationState -> [RMove]
playable r rs = availableMoves r $ scores rs !! length (accumulating rs)

-- | A player makes a move.
registerMove :: RealizationState -> RMove -> RealizationState
registerMove rs mv = if length (accumulating newRS) == length (scores newRS)
                     then progress newRS
                     else newRS
    where newRS = RS (scores rs) (mv: accumulating rs)
          progress rs = let step p mv  = SS (mv: realization p) (tail $ future p)
                            newPlayers = zipWith step (scores rs) (reverse $ accumulating rs)
                        in RS newPlayers []

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
  type Move  Improvise = RMove
  type State Improvise = RealizationState
  
  gameTree (Imp payoff state range) = 
    stateTreeD who end (playable range) registerMove payoff state

