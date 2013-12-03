{-# LANGUAGE FlexibleContexts #-}
module Strategy where
import Game
import State
import Translations
import Hagl
import Control.Monad (liftM)

type Interval = Int
type IntPreference = (Interval, Float)

intPref :: [IntPreference] -> Int -> Float
intPref prefs i = foldr f 0 prefs
    where f (interval, pay) acc = if i == interval then pay + acc else acc

rmoveInterval :: RMove -> RMove -> Maybe Interval
rmoveInterval (Begin p1)  (Begin p2)  = Just $ interval p1 p2
rmoveInterval (Begin p1)  (Extend p2) = Just $ interval p1 p2
rmoveInterval (Extend p1) (Begin p2)  = Just $ interval p1 p2
rmoveInterval (Extend p1) (Extend p2) = Just $ interval p1 p2
rmoveInterval _           _           = Nothing

onePlayerPay :: [RMove] -> [[RMove]] -> [IntPreference] -> Float
onePlayerPay [] _ _ = 0
onePlayerPay _ [] _ = 0
onePlayerPay (me:rs) others ps = foldr f 0 others + onePlayerPay rs (map tail others) ps
    where f (m:ms) acc = case rmoveInterval me m of
                            Nothing -> acc
                            Just a  -> acc + intPref ps a

intervalPayoff :: [[IntPreference]] -> RealizationState -> Payoff
intervalPayoff prefs rs = ByPlayer $ p [] (scores rs) prefs
    where p _      []         _     = []
          p before (me:after) (myPrefs:ps) = 
              onePlayerPay (realization me) (map realization (before ++ after)) myPrefs: 
              p (me:before) after ps




myScore :: DiscreteGame Improvise => Strategy () Improvise
myScore = liftM myScoreAlg location

myScoreAlg :: Discrete RealizationState RMove -> RMove
myScoreAlg (Discrete ((RS scores accum),_) _) = 
    let SS _ future = scores !! (length accum)
     in head future


