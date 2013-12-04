module Conversions where

import Euterpea
import State 
import Game
import Hagl
import Debug.Trace
import Data.Ratio

{- 
 
 This module contains code necessary for converting between Euterpea's
 representation of playable music and Improvise's representation of music as a  
 series of fixed-duration musical events (Rmove).

 -}

-- | The duration of each musical event.
baseDur :: Dur
baseDur = 1/8

-- 
-- * Converting from Improvise Rmove to Euterpea Music
--

-- | Reconstruct the realization state from the move summary of a 
-- completed game.
getRS :: MoveSummary (Move Improvise) -> RealizationState
getRS mss = RS (map score (everyPlayer mss)) []
    where score player = SS (everyTurn player) []  

-- | Convert from an individual's realization of the score in the game to a 
-- series of playable notes in Euterpea.
ssToMusic :: SingularScore -> Music Pitch
ssToMusic (SS realization future) = 
    let condenseMove ((State.Rest, x):l) State.Rest  = (State.Rest, x + 1):l
        condenseMove ((Begin p1, x):l)   (Extend p2) = 
            if p1 == p2
            then (Begin p1, x+1):l
            else error "Extend must extend same pitch as most recent pitch"
        condenseMove l mv                 = (mv,1):l
        condensed                         = foldl condenseMove [] (reverse realization ++ future)
        condensedToMusic (State.Rest, d)  = Prim (Euterpea.Rest (d*baseDur))
        condensedToMusic (Begin p, d)     = Prim (Note (d*baseDur) p) 
        musicMoves                        = map condensedToMusic condensed
    in  foldr (:+:) (Prim (Euterpea.Rest 0)) (reverse musicMoves)

-- | Overlay each players' individual realization to produce one peice of 
-- playable music. 
rsToMusic :: RealizationState -> Music Pitch
rsToMusic (RS players _) = 
    foldr ((:=:) . ssToMusic) (Prim (Euterpea.Rest 0)) players


-- 
-- * Converting from Euterpea Music to Improvise Rmove
--

-- | Ensure the musical event duration is among a set of sane durations.
okDur :: Dur -> Bool
okDur d = denominator d `elem` [1,2,4,8]

-- | Convert between a musical event with a given duration to a series of
-- fixed-duration musical events representing the same sound.
genRMoves :: RMove -> Dur -> [RMove]
genRMoves r d | okDur d   = replicate (floor (d * (1/baseDur))) r 
              | otherwise = error "invalid Duration"

-- | Contruct a list of Improvise musical events from Euterpea music.
musicToRMoves :: Music Pitch -> [RMove]
musicToRMoves (Prim (Note d p))        = Begin p: 
                                         genRMoves (Extend p) (d-baseDur)
musicToRMoves (Prim (Euterpea.Rest d)) = genRMoves State.Rest d
musicToRMoves (m1 :+: m2)              = musicToRMoves m1 ++ musicToRMoves m2
musicToRMoves (m1 :=: m2)              = let c1 = musicToRMoves m1
                                             c2 = musicToRMoves m2
                                             merge :: RMove -> RMove -> RMove
                                             merge State.Rest x          = x
                                             merge x          State.Rest = x
                                             merge _          _          = error "cannot parse overlay"
                                          in if (length c1) > (length c2)
                                             then (zipWith merge (take (length c2) c1) c2) ++ drop (length c2) c1
                                             else (zipWith merge (take (length c1) c2) c1) ++ drop (length c1) c2
musicToRMoves (Modify c m1)            = 
    trace ("Warning: discarding " ++ show c) musicToRMoves m1

-- | Construct an individual score from Euterpea music.
musicToSS :: Music Pitch -> SingularScore
musicToSS m = SS [] (musicToRMoves m)

