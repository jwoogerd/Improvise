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
           | Extend deriving Show
           -- invariant: Extend implies extending a previous "Begin"

type Player = Int
type RealizationState = [([RMove], [RMove])]
  -- A list of tuples, each one representing a player
     -- their first list represents the realization of their score at this point in the game
     -- their second list represents the remainder of the score

start :: [[RMove]]
start = [[Begin (C, 5), Main.Rest, Begin (D, 5)], [Begin (A, 5), Extend, Main.Rest]]
-- find a way to assert that inner lists are the same length

progress :: RealizationState -> [RMove] -> RealizationState
progress []              []       = []
progress ((r, _:sc):ps) (mv:mvs) = (mv:r,sc) : progress ps mvs

possMoves :: [RMove] -> [RMove]
possMoves (Begin p:prev) = generateMoves p ++ [Main.Rest, Extend]
possMoves (_      :prev) = take (2*range) $ possMoves prev
possMoves  _             = [Main.Rest]

-- returns a list of RMoves range number of halfsteps above & below p
generateMoves :: Pitch -> [RMove]
generateMoves p =
    let genMoves _ _ 0 = []
        genMoves p f n = let m = f p
                         in Begin m : genMoves m f (n-1)
    in genMoves p halfStepUp range ++ genMoves p halfStepDown range

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
                            
octv :: Octave
octv = 5
main = do { putStrLn "Just MG" ;
            Euterpea.play (Prim (Note 1 (Ass, octv)))}
