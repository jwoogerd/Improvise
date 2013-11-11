{-# LANGUAGE FlexibleContexts, TypeFamilies #-}


import Control.Monad.Trans (liftIO)
import Data.List           (elemIndices, intersperse, transpose)
import Text.Printf         (printf)
import Hagl
import Euterpea
import Translations

range = 5

data RMove = Begin Pitch
           | Rest
           | Extend
           -- invariant: Extend implies extending a previous "Begin"

start :: [[RMove]]
start = [[Begin (C, 5), Main.Rest, Begin (D, 5)], [Begin (A, 5), Extend, Main.Rest]]
-- find a way to assert that inner lists are the same length

possMoves :: [RMove] -> [RMove]
possMoves l@((Begin p):prev) = rangedMoves l ++ [Main.Rest, Extend]
possMoves l                  = rangedMoves l

rangedMoves :: [RMove] -> [RMove]
rangedMoves ((Begin p) : prev) = upperMoves p range ++ lowerMoves p range
rangedMoves (_         : prev) = rangedMoves prev
rangedMoves []                 = [Main.Rest]


upperMoves :: Pitch -> Int -> [RMove]
upperMoves p i = case i of 0 -> []
                           _ -> let u = halfStepUp p 
                                in  Begin u : upperMoves u (i - 1)
 
lowerMoves :: Pitch -> Int -> [RMove]
lowerMoves p i = case i of 0 -> []
                           _ -> let u = halfStepDown p 
                                in  Begin u : lowerMoves u (i - 1)
 

main = do { putStrLn "yo" }

