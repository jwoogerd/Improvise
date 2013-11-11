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
type RealizationState = [([RMove], [RMove])]
  -- A list of tuples, each one representing a player
     -- their first list represents the realization of their score at this point in the game
     -- their second list represents the remainder of the score

progress :: RealizationState -> [RMove] -> RealizationState
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
 
data Improvise = Imp (Simultaneous RMove) RealizationState 
instance Game Improvise where
  
  type TreeType Improvise = Continuous
  type State    Improvise = RealizationState
  type Move     Improvise = RMove

  gameTree (Imp (Simultaneous numPlayers validMove f) rs) = tree 1 []
    where
      tree p ms
        | p <= numPlayers = Continuous (rs, Decision p)
                            (\rmove -> if validMove p rmove
                                       then Just (tree (p+1) (rmove:ms))
                                       else Nothing)
        | otherwise       = let newState = progress rs (reverse ms)
                            in Continuous (newState, (Payoff . f . ByPlayer . reverse) ms)
                            (\_ -> Nothing)
                            
main = do { putStrLn "yo" }

