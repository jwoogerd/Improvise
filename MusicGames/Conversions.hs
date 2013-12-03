module Conversions where

import Euterpea
import State 
import Game
import Hagl
import Debug.Trace
import Data.Ratio

baseDur :: Dur
baseDur = 1/8

-- convert from our representation to Euterpea Music
getRS :: MoveSummary (Move Improvise) -> RealizationState
getRS mss = RS (map (\player -> SS (reverse (everyTurn player)) []) (everyPlayer mss)) []

rsToMusic :: RealizationState -> Music Pitch
rsToMusic (RS players _) = foldr (:=:) (Prim (Euterpea.Rest 0)) (map ssToMusic players) 


ssToMusic :: SingularScore -> Music Pitch
ssToMusic (SS realization future) = 
    let condenseMove ((State.Rest, x):l)   State.Rest       = (State.Rest, x + 1):l
        condenseMove ((Begin p1, x):l)   (Extend p2)      = 
            if p1 == p2
            then (Begin p1, x+1):l
            else error "Extend must extend same pitch as most recent pitch"
        condenseMove l mv                                 = (mv,1):l
        condensed                                         = foldl condenseMove [] realization
        condensedToMusic (State.Rest, d)                   = Prim (Euterpea.Rest (d*baseDur))
        condensedToMusic (Begin p, d)                     = Prim (Note (d*baseDur) p) 
        musicMoves                                        = map condensedToMusic condensed
    in  foldr (:+:) (Prim (Euterpea.Rest 0)) (reverse musicMoves)

-- convert from Euterpea Music to our representation
okDur :: Dur -> Bool
okDur d = elem (denominator d) [1,2,4,8]

genRMoves :: RMove -> Dur -> [RMove]
genRMoves r d = if okDur d then take (floor (d * (1/baseDur))) (repeat r) else error "invalid Duration"

musicToRMoves :: Music Pitch -> [RMove]
musicToRMoves (Prim (Note d p))          = Begin p : (genRMoves (Extend p) (d - baseDur))
musicToRMoves (Prim (Euterpea.Rest d  )) = genRMoves  State.Rest d
musicToRMoves (m1 :+: m2)                = (musicToRMoves m1) ++ (musicToRMoves m2)
musicToRMoves (m1 :=: m2)                = error "Cannot parse overlay"
musicToRMoves (Modify c m1)              = trace ("Warning: discarding " ++ show c) musicToRMoves m1
  
musicToSS :: Music Pitch -> SingularScore
musicToSS m = SS [] (reverse (musicToRMoves m))



