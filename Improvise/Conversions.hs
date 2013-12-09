module Conversions where

import Euterpea hiding (Performance) 
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
getPerformance :: MoveSummary (Move Improvise) -> Performance
getPerformance mss = ByPlayer (map score (everyPlayer mss))
    where score player = Performer (everyTurn player) []  

-- | Convert from an individual's realization of the score in the game to a 
-- series of playable notes in Euterpea.
ssToMusic :: Performer -> Music Pitch
ssToMusic (Performer realization future) = 
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
performanceToMusic :: Performance -> Music Pitch
performanceToMusic (ByPlayer performers) = 
    foldr ((:=:) . ssToMusic) (Prim (Euterpea.Rest 0)) performers


-- 
-- * Converting from Euterpea Music to Improvise Rmove
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
musicToMusicMvs (Prim (Euterpea.Rest d)) = genMusicMvs State.Rest d
musicToMusicMvs (m1 :+: m2)              = musicToMusicMvs m1 ++ musicToMusicMvs m2
musicToMusicMvs (m1 :=: m2)              = let c1 = musicToMusicMvs m1
                                               c2 = musicToMusicMvs m2
                                               merge :: MusicMv -> MusicMv -> MusicMv
                                               merge State.Rest x          = x
                                               merge x          State.Rest = x
                                               merge _          _          = error "cannot parse overlay"
                                            in if (length c1) > (length c2)
                                               then (zipWith merge (take (length c2) c1) c2) ++ drop (length c2) c1
                                               else (zipWith merge (take (length c1) c2) c1) ++ drop (length c1) c2
musicToMusicMvs (Modify c m1)            =  musicToMusicMvs m1
    --trace ("Warning: discarding " ++ show c) musicToMusicMvs m1

-- | Construct an individual score from Euterpea music.
musicToPerformer :: Music Pitch -> Performer
musicToPerformer m = Performer [] (musicToMusicMvs m)


-- | Extends singular scores with rests so that lengths match
extendPerformers :: Performance -> Performance
extendPerformers (ByPlayer performers) = 
    let len                  = maximum $ map (length . future) performers
        extend (Performer [] future) = let extension = replicate (len - (length future)) State.Rest
                                in Performer [] (future ++ extension)
        extend (Performer  _ future) = error "cannot extend score after start of game"
     in ByPlayer (map extend performers)


