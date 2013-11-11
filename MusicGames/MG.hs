{-# LANGUAGE FlexibleContexts, TypeFamilies #-}


import Control.Monad.Trans (liftIO)
import Data.List           (elemIndices, intersperse, transpose)
import Text.Printf         (printf)
import Hagl
import Euterpea
import Translations

range = 6

data RMove = Begin Pitch
           | Rest
           | Extend
           -- invariant: Extend implies extending a previous "Begin"

start :: [[RMove]]
start = [[Begin (C, 5), Main.Rest, Begin (D, 5)], [Begin (A, 5), Extend, Main.Rest]]
-- find a way to assert that inner lists are the same length

type Player = Int
type RealizationState = [([RMoves], [RMoves])]
  -- A list of tuples, each one representing a player
     -- their first list represents the realization of their score at this point in the game
     -- their second list represents the remainder of the score

progress :: RealizationState -> [RMoves] -> RealizationState
progress []              []       = []
progress ((r,(_:sc)):ps) (mv:mvs) = (mv:r,sc) : (progress ps mvs)


possMoves :: [RMove] -> [RMove]
possMoves l@((Begin p):prev) = rangedMoves l ++ [Main.Rest, Extend]
possMoves l                  = rangedMoves l

rangedMoves :: [RMove] -> [RMove]
rangedMoves ((Begin p) : prev) = upperMoves p range ++ lowerMoves p range
rangedMoves (_         : prev) = rangedMoves prev
rangedMoves []                 = [Main.Rest]


upperMoves :: Pitch -> Int -> [RMove]
upperMoves p i = case i of 0 -> []
                           _ -> let u = halfStepUp p 
                                in  Begin u : upperMoves u (i - 1)
 
lowerMoves :: Pitch -> Int -> [RMove]
lowerMoves p i = case i of 0 -> []
                           _ -> let u = halfStepDown p 
                                in  Begin u : lowerMoves u (i - 1)
 

type Improvise 
instance IsSimultaneous Improvise RMove where
  toSimultaneous :: Improvise -> Simultaneous RMove
  toSimultaneous 

  Simultaneous RMove = Simultaneous Int (PlayerID -> RMove -> Bool) (Profile RMove -> Payoff)


type Improvise = Imp (Simultaneous RMove) RealizationState -- State?
instance Game Improvise where
  
  type TreeType Improvise = Discrete
  type State    Improvise = RealizationState
  type Move     Improvise = RMove

  gameTree :: Improvise -> (TreeType Improvise) (State Improvise) (Move Improvise) 
  gameTree (Imp (Simultaneous numPlayers validMove f) RS) = tree 1 []
    where
      tree p ms
        | p <= numPlayers = Continuous (RS, Decision p)
                            (\rmove -> if validMove p rmove
                                       then Just (tree (p+1) (rmove:ms))
                                       else Nothing)
        | otherwise       = let newState = progress RS (reverse ms)
                            in Continuous (newState, (Payoff . f . ByPlayer . reverse) ms)
                            (\_ -> Nothing)
                            
-- | Build a discrete game tree for a state-based game.
stateTreeD ::
        (s -> PlayerID) -- who   ^ Whose turn is it?
     -> (s -> Bool)     -- end   ^ Is the game over?
     -> (s -> [mv])     -- moves ^ Available moves.
     -> (s -> mv -> s)  -- exec  ^ Execute a move and return the new state.
     -> (s -> Payoff)   -- pay   ^ Payoff for this (final) state.
     -> s               -- s     ^ The current state.
     -> Discrete s mv
stateTreeD who end moves exec pay = tree
  where tree s | end s     = Discrete (s, Payoff (pay s)) []
               | otherwise = Discrete (s, Decision (who s)) [(m, tree (exec s m)) | m <- moves s]



main = do { putStrLn "yo" }

