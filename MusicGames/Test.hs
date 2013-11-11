{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

{- Some experiments with Hagl -}

module MusicGames.Test where

import Euterpea
import Hagl

data Note = An | Bn | Rn deriving (Eq, Show)

testScore :: [[Note]]
testScore = [[An, Bn], [An, An], [Rn, Bn], [Bn, Bn], [An, Rn]]

type Improv = (Note -> Note)

same :: Improv
same note = note

swap :: Improv
swap An = Bn
swap Bn = An
swap Rn = Rn

testMoves :: [[Improv]]
testMoves = [[same, same], [same, swap], [swap, same], [swap, swap], [swap, same]]

transform :: [Note] -> [Improv] -> [Note]
transform [] [] = []
transform (n:ns) (m:ms) = m n:transform ns ms

newScore :: [[Note]] -> [[Improv]] -> [[Note]]
newScore [] [] = []
newScore (e:es) (m:ms) = transform e m:newScore es ms

score2 = newScore testScore testMoves
