{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Game where

import Euterpea (Pitch)
import Hagl

--
-- * Game representation 
--

-- | An Improvise move is a discrete, fixed-duration musical event. A player 
-- may start playing a new note of any pitch, continue or extend the previous 
-- note, or rest for the duration. 
-- Invariants: Extend implies extending a previous Begin; an Extend may not
--             follow a Rest
--             Extend must have the same pitch as the most recent Begin
data MusicMv = Begin Pitch
             | Extend Pitch 
             | Rest deriving Eq


-- | A Performer represents an individual player. The realization is a 
-- list of musical events that have already occurred, i.e. the player's
-- interpretation of the score thus far (most recent event first). The future 
-- is a list of the upcoming events, i.e. the remaining portion of her score.  
data Performer = Performer { realization :: [MusicMv]
                           , future      :: [MusicMv] } deriving Show


-- | A Performance represents the state of the game at any moment. 
type Performance = ByPlayer Performer


-- | The Improvise data type wraps information unique to a particular game 
-- execution: the initial game state (the scores); a description of the 
-- players' musical aesthetic preferences (i.e. a function to generate 
-- payoffs);  and a function for determining legal moves from the given state. 
data Improvise = Imp { state    :: Performance
                     , payoff   :: Performance -> Payoff
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
            | otherwise = error "Internal game tree error."


--
-- * Show instances (TODO: move this?)
--

instance Show MusicMv where --use 20 chars
    show (Begin p)  = take 20 $ "Begin  " ++ show p ++ repeat ' '
    show (Extend p) = take 20 $ "Extend " ++ show p ++ repeat ' '
    show Game.Rest  = take 20 $ "Rest" ++ repeat ' '
                            
