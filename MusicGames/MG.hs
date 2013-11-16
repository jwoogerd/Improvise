{-# LANGUAGE FlexibleContexts, TypeFamilies #-}


import Control.Monad.Trans (liftIO)
import Data.List           (elemIndices, intersperse, transpose)
import Text.Printf         (printf)
import Hagl
import Euterpea
import Translations
import Debug.Trace

range = 2

data RMove = Begin Pitch
           | Rest
           | Extend Pitch deriving (Show, Eq)
           -- invariant: Extend implies extending a previous "Begin"
           -- invariant: Extend must have the same pitch as the most recent "Begin"

data SingularScore = SS { realization :: [RMove], future :: [RMove] } deriving Show
data RealizationState = RS { scores :: [SingularScore], accumulating :: [RMove] } deriving Show


player1 :: SingularScore
player1 = SS { realization = [], future = [Begin (C,5), Main.Rest, Begin (D,5)] }
player2 :: SingularScore
player2 = SS { realization = [], future = [Begin (A,5), Extend (A,5), Main.Rest] }

start :: RealizationState
start = RS { scores = [player1,player2] , accumulating = [] }


registerMove :: RealizationState -> RMove -> RealizationState
registerMove rs mv = let newRS = RS { scores = (scores rs), 
                                      accumulating = (mv : (accumulating rs))}
                      in if (length (accumulating newRS) == length (scores newRS)) 
                         then progress newRS
                         else newRS

progress :: RealizationState -> RealizationState
progress rs = let newPlayers = progressHelper (scores rs) (reverse (accumulating rs))
               in RS {scores = newPlayers, accumulating = []}


progressHelper :: [SingularScore] -> [RMove] -> [SingularScore]
progressHelper [] [] = []
progressHelper (p:ps) (mv:mvs) = (SS { realization = mv:(realization p), 
                                       future      = (drop 1 (future p))})
                                 : (progressHelper ps mvs)

markable :: RealizationState -> [RMove]
markable rs = possMoves ( (scores rs) !! (length (accumulating rs)))
--markable rs = [Begin (A,5)]

possMoves :: SingularScore -> [RMove]
possMoves SS { realization = _              , future = [] }           = []
possMoves SS { realization = m@(Begin r:rs) , future = (Begin f):fs } = generateMoves f ++ rangedMoves m ++ [Extend r, Main.Rest]
possMoves SS { realization = m              , future = (Begin f):fs } = generateMoves f ++ rangedMoves m ++           [Main.Rest]
possMoves SS { realization = m@(Begin r:rs) , future = _ }            =                    rangedMoves m ++ [Extend r, Main.Rest]
possMoves SS { realization = m              , future = _ }            =                    rangedMoves m ++           [Main.Rest]
-- TODO UNION THE LISTS!!!


rangedMoves :: [RMove] -> [RMove]
rangedMoves ((Begin p) :prev) = generateMoves p
rangedMoves (       _  :prev) = rangedMoves prev
rangedMoves                 _ = []                            

who :: RealizationState -> PlayerID
who rs = length (accumulating rs) + 1


-- returns a list of RMoves range number of halfsteps above & below p
generateMoves :: Pitch -> [RMove]
generateMoves p =
    let genMoves _ _ 0 = []
        genMoves p f n = let m = f p
                         in Begin m : genMoves m f (n-1)
    in genMoves p halfStepUp range ++ [Begin p] ++ genMoves p halfStepDown range


end :: RealizationState -> Bool
end rs = null (accumulating rs) && null (future (head (scores rs)))

x :: Float
x = 1.0
pay :: RealizationState -> Payoff
pay rs = ByPlayer (take (length (scores rs)) (repeat x))

data Improvise = Improvise

-- Game instance
instance Game Improvise where
  type TreeType Improvise = Discrete
  type Move  Improvise = RMove
  type State Improvise = RealizationState
  gameTree _ = stateTreeD who end markable registerMove pay start

printGame :: GameM m Improvise => m ()
printGame = gameState >>= liftIO . putStrLn . show

guessPlayers :: [Hagl.Player Improvise]
guessPlayers = ["A" ::: return Main.Rest, "B" ::: return Main.Rest]

main = evalGame Improvise guessPlayers (run >> printTranscript)
   where run = printGame >> step >>= maybe run (\p -> printGame >> return p)



--octv :: Octave
--octv = 5
--main = do { putStrLn "Just MG" ;
--            Euterpea.play (Prim (Note 1 (Ass, octv)))}
