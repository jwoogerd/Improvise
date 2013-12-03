{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Game where
import Euterpea
import Hagl
import State
import Moves


who :: RealizationState -> PlayerID
who rs = length (accumulating rs) + 1

end :: RealizationState -> Bool
end (RS scores accumulating) = null accumulating && null (future (head scores))

markable :: RealizationState -> [RMove]
markable rs = possMoves $ scores rs !! length (accumulating rs)
  
registerMove :: RealizationState -> RMove -> RealizationState
registerMove rs mv = if length (accumulating newRS) == length (scores newRS)
                     then progress newRS
                     else newRS
    where newRS = RS (scores rs) (mv: accumulating rs)
          progress rs = let step p mv  = SS (mv: realization p) (tail $ future p)
                            newPlayers = zipWith step (scores rs) (reverse $ accumulating rs)
                        in RS newPlayers []



data Improvise = Imp { payoff :: RealizationState -> Payoff
                     , start  :: RealizationState           }
    
-- Game instance
instance Game Improvise where
  type TreeType Improvise = Discrete
  type Move  Improvise = RMove
  type State Improvise = RealizationState
  
  gameTree (Imp payoff start) = stateTreeD who end markable registerMove payoff start


