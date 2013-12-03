module Moves where
import Translations
import State
import Euterpea (Pitch)


range = 2

possMoves :: SingularScore -> [RMove]
possMoves (SS _               []         ) = []
possMoves (SS m@(Begin r:rs) (Begin f:fs)) = Rest: Extend r: rangedMoves m ++ generateMoves f 
possMoves (SS m              (Begin f:fs)) = Rest:           rangedMoves m ++ generateMoves f
possMoves (SS m@(Begin r:rs)  _          ) = Rest: Extend r: rangedMoves m
possMoves (SS m@(Extend r:rs) _          ) = Rest: Extend r: rangedMoves m
possMoves (SS m               _          ) = Rest:           rangedMoves m

rangedMoves :: [RMove] -> [RMove]
rangedMoves (Begin p:prev) = generateMoves p
rangedMoves (_      :prev) = rangedMoves prev
rangedMoves _              = []                            


-- returns a list of RMoves range number of halfsteps above & below p
generateMoves :: Pitch -> [RMove]
generateMoves p =
    let genMoves _ _ 0 = []
        genMoves p f n = let m = f p
                         in Begin m: genMoves m f (n-1)
    in Begin p: genMoves p halfStepUp range ++ genMoves p halfStepDown range



