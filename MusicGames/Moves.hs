module Moves where
import Translations
import State
import Euterpea (Pitch)

{-

This module contains code for generating sets of possible moves (i.e. improvised
deviations) from a prescribed musical event given by the score.

Deviation from a given pitch is limited to within an integer range of pitches 
above and below it. A larger range increases the freedom for a player to 
improvise, but also increases the size of the game tree.  

-}

-- | Generate a list of possible moves from a given range and score. We  
-- enforce the following invariants for possible moves:
--  TODO: nail down invariants
possMoves :: Int -> SingularScore -> [RMove]
possMoves i ss = case ss of 
  (SS _               [])          -> []
  (SS m@(Begin r:rs) (Begin f:fs)) -> Rest: Extend r: 
                                            rangedMoves i m ++ generateMoves i f
  (SS m              (Begin f:fs)) -> Rest: rangedMoves i m ++ generateMoves i f
  (SS m@(Begin r:rs)  _ )          -> Rest: Extend r: rangedMoves i m
  (SS m@(Extend r:rs) _ )          -> Rest: Extend r: rangedMoves i m
  (SS m               _ )          -> Rest:           rangedMoves i m

-- | Produce a list of moves from the most recent past musical event.
rangedMoves :: Int -> [RMove] -> [RMove]
rangedMoves r (Begin p:prev) = generateMoves r p
rangedMoves r (_      :prev) = rangedMoves r prev
rangedMoves _ _              = []                            

-- | For a given range and pitch, generate a list of moves (Begins) range 
-- number of half steps above and below that pitch.
generateMoves :: Int -> Pitch -> [RMove]
generateMoves range p =
    let genMoves _ _ 0 = []
        genMoves p f n = let m = f p
                         in Begin m: genMoves m f (n-1)
    in Begin p: genMoves p halfStepUp range ++ genMoves p halfStepDown range

