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
           | Extend deriving (Show, Eq)
           -- invariant: Extend implies extending a previous "Begin"

type RealizationState = [([RMove], [RMove])]
  -- A list of tuples, each one representing a player
     -- their first list represents the realization of their score at this point in the game
     -- their second list represents the remainder of the score

start :: RealizationState
start = [([],[Begin (C,5), Main.Rest, Begin (D,5)]),([],[Begin (A,5), Extend, Main.Rest])]
--start = [([],[Begin (C,5)]),([],[Begin (A,5)])]
-- find a way to assert that inner lists are the same length

progress :: RealizationState -> [RMove] -> RealizationState
progress []              []       = []
progress ((r, _:sc):ps) (mv:mvs) = (mv:r,sc) : progress ps mvs

possMoves :: [RMove] -> [RMove]
possMoves m@(Begin p:prev) = rangedMoves m ++ [Main.Rest, Extend]
possMoves                m = rangedMoves m


rangedMoves :: [RMove] -> [RMove]
rangedMoves ((Begin p) :prev) = generateMoves p
rangedMoves (       _  :prev) = rangedMoves prev
rangedMoves                 _ = [Main.Rest]                            



-- returns a list of RMoves range number of halfsteps above & below p
generateMoves :: Pitch -> [RMove]
generateMoves p =
    let genMoves _ _ 0 = []
        genMoves p f n = let m = f p
                         in Begin m : genMoves m f (n-1)
    in genMoves p halfStepUp range ++ genMoves p halfStepDown range

end :: RealizationState -> Bool
end rs@((_,[]):_) = trace ("True " ++ show rs) True
end rs          = trace ("False " ++ show rs) False

data Improvise = Imp (Simultaneous RMove) RealizationState 
instance Game Improvise where
  
  type TreeType Improvise = Continuous
  type State    Improvise = RealizationState
  type Move     Improvise = RMove

  gameTree t@(Imp (Simultaneous numPlayers validMove f) rs) = trace ("YO " ++ show rs) (tree 1 [])
    where
      tree p ms
        | p <= numPlayers = Continuous (rs, Decision p)
                            (\rmove -> if trace ("I AM PLAYER " ++ show p) (validMove p rmove)
                                       then Just (tree (p+1) (rmove:ms))
                                       else Nothing)
        | otherwise       = let newState = progress rs (reverse ms)
                            in trace "TOO MANY PLAYERS, YO" (Continuous (newState, (Payoff . f . ByPlayer . reverse) ms)
                            (\_ -> if end newState
                                   then Nothing
                                   else Just (gameTree (Imp (Simultaneous numPlayers validMove f) newState))))
getPayoff:: Profile RMove -> Payoff
getPayoff (ByPlayer _) = ByPlayer [1,0]


testGame = Imp (Simultaneous 2 (\x -> \y -> True) Main.getPayoff) start

guessPlayers :: [Hagl.Player Improvise]
guessPlayers = ["A" ::: return Main.Rest, "B" ::: return Main.Rest]
                            
execute = evalGame testGame guessPlayers (run >> printTranscript)
   where run = printGame >> step >>= maybe run (\p -> printGame >> return p)

--playTicTacToe = evalGame guessPlayers
--                         (run >> printScore)
--  where run = printGame >> step >>= maybe run (\p -> printGame >> return p)

 

printGame :: GameM m Improvise => m ()
printGame = gameState >>= liftIO . putStrLn . show

--octv :: Octave
--octv = 5
--main = do { putStrLn "Just MG" ;
--            Euterpea.play (Prim (Note 1 (Ass, octv)))}

