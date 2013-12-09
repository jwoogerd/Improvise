module State where
import Euterpea (Pitch)
import Hagl     (ByPlayer)

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


instance Show MusicMv where --use 20 chars
    show (Begin p)  = take 20 $ "Begin  " ++ show p ++ repeat ' '
    show (Extend p) = take 20 $ "Extend " ++ show p ++ repeat ' '
    show State.Rest = take 20 $ "Rest" ++ repeat ' '
                            
