module Conversions where

import Euterpea hiding (Performance) 
import Game
import Hagl
import Debug.Trace
import Data.Ratio

{- 
 
 This module contains code necessary for converting between Euterpea's
 representation of playable music and Improvise's representation of music as a  
 series of fixed-duration musical events (MusicMv).

 -}

-- | The duration of each musical event.
baseDur :: Dur
baseDur = 1/8

-- 
-- * Converting from Improvise MusicMv to Euterpea Music
--

-- | Reconstruct the realization state from the move summary of a 
-- completed game.
getPerformance :: MoveSummary (Move Improvise) -> Performance
getPerformance mss = ByPlayer (map score (everyPlayer mss))
    where score player = Performer (everyTurn player) []  

-- | Convert from an individual's realization of the score in the game to a 
-- series of playable notes in Euterpea.
performerToMusic :: Performer -> Music Pitch
performerToMusic (Performer realization future) = 
    let absorbMove ((Game.Rest, x):l)  Game.Rest  = (Game.Rest, x + 1):l
        absorbMove ((Begin p1,  x):l) (Extend p2) = 
            if p1 == p2
            then (Begin p1, x+1):l
            else error "Extend must extend same pitch as most recent pitch"
        absorbMove l mv                           = (mv,1):l
        condensed                         = foldl absorbMove [] (reverse realization ++ future)
        condensedToMusic (Game.Rest, d)   = Prim (Euterpea.Rest (d*baseDur))
        condensedToMusic (Begin p, d)     = Prim (Note (d*baseDur) p) 
        musicMoves                        = map condensedToMusic condensed
    in  foldr (:+:) (Prim (Euterpea.Rest 0)) (reverse musicMoves)

-- | Overlay each players' individual realization to produce one peice of 
-- playable music. 
performanceToMusic :: Performance -> Music Pitch
performanceToMusic (ByPlayer performers) = 
    foldr ((:=:) . performerToMusic) (Prim (Euterpea.Rest 0)) performers


-- 
-- * Converting from Euterpea Music to Improvise MusicMv
--

-- | Ensure the musical event duration is among a set of sane durations.
okDur :: Dur -> Bool
okDur d = denominator d `elem` [1,2,4,8]

-- | Convert between a musical event with a given duration to a series of
-- fixed-duration musical events representing the same sound.
genMusicMvs :: MusicMv -> Dur -> [MusicMv]
genMusicMvs r d | okDur d   = replicate (floor (d * (1/baseDur))) r 
                | otherwise = error "invalid Duration"

-- | Contruct a list of Improvise musical events from Euterpea music.
musicToMusicMvs :: Music Pitch -> [MusicMv]
musicToMusicMvs (Prim (Note d p))        = Begin p : genMusicMvs (Extend p) (d-baseDur)
musicToMusicMvs (Prim (Euterpea.Rest d)) = genMusicMvs Game.Rest d
musicToMusicMvs (m1 :+: m2)              = musicToMusicMvs m1 ++ musicToMusicMvs m2
musicToMusicMvs (m1 :=: m2)              = let c1 = musicToMusicMvs m1
                                               c2 = musicToMusicMvs m2
                                               merge :: MusicMv -> MusicMv -> MusicMv
                                               merge Game.Rest x          = x
                                               merge x          Game.Rest = x
                                               merge _          _          = error "cannot parse overlay"
                                               (long, short) = if length c1 > length c2
                                                               then (c1, c2)
                                                               else (c2, c1)
                                               shortLen = length short
                                            in (zipWith merge (take shortLen long) short) ++ drop shortLen long
musicToMusicMvs (Modify c m1)            =  musicToMusicMvs m1

-- | Construct an individual score from Euterpea music.
musicToPerformer :: Music Pitch -> Performer
musicToPerformer m = Performer [] (musicToMusicMvs m)


-- | Extends performers with rests so that lengths match
extendPerformers :: Performance -> Performance
extendPerformers (ByPlayer performers) = 
    let len                  = maximum $ map (length . future) performers
        extend (Performer [] future) = let extension = replicate (len - (length future)) Game.Rest
                                in Performer [] (future ++ extension)
        extend (Performer  _ future) = error "cannot extend score after start of game"
     in ByPlayer (map extend performers)



