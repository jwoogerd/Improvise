{-# LANGUAGE FlexibleContexts, TypeFamilies #-}


import Control.Monad.Trans (liftIO)
import Control.Monad (liftM,liftM2,unless)
import Hagl.History
import Hagl
import Euterpea
import Translations
import Debug.Trace

-- test data and constants (probably to be removed later)
octv :: Octave
octv = 5

dummyPayoff :: Float
dummyPayoff = 1.0

baseDur :: Dur
baseDur = 1/8

range = 2

player1 :: SingularScore
player1 = SS [] [Begin (C,5), Main.Rest, Begin (D,5)]
player2 :: SingularScore
player2 = SS [] [Begin (A,5), Extend (A,5), Main.Rest]

--
-- Data definitions
--

data Improvise = Improvise

data RMove = Begin Pitch
           | Rest
           | Extend Pitch deriving (Show, Eq)
           -- invariant: Extend implies extending a previous "Begin"
           -- invariant: Extend must have the same pitch as the most recent "Begin"

data SingularScore = SS { realization :: [RMove], 
                          future      :: [RMove] } 
                          deriving Show

data RealizationState = RS { scores       :: [SingularScore], 
                             accumulating :: [RMove] } 
                             deriving Show


start :: RealizationState
start = RS [player1, player2] []

who :: RealizationState -> PlayerID
who rs = length (accumulating rs) + 1

markable :: RealizationState -> [RMove]
markable rs = possMoves $ scores rs !! length (accumulating rs)
--markable rs = [Begin (A,5)]

registerMove :: RealizationState -> RMove -> RealizationState
registerMove rs mv = let newRS = RS (scores rs) (mv : accumulating rs)
                     in if length (accumulating newRS) == length (scores newRS)
                        then progress newRS
                        else newRS

progress :: RealizationState -> RealizationState
progress rs = let newPlayers = progressHelper (scores rs) (reverse (accumulating rs))
              in RS newPlayers []


progressHelper :: [SingularScore] -> [RMove] -> [SingularScore]
progressHelper []     []       = []
progressHelper (p:ps) (mv:mvs) = (SS (mv:realization p) (drop 1 (future p))) : progressHelper ps mvs

possMoves :: SingularScore -> [RMove]
possMoves (SS _               []         ) = []
possMoves (SS m@(Begin r:rs) (Begin f:fs)) = generateMoves f ++ rangedMoves m ++ [Extend r, Main.Rest]
possMoves (SS m              (Begin f:fs)) = generateMoves f ++ rangedMoves m ++           [Main.Rest]
possMoves (SS m@(Begin r:rs)  _          ) =                    rangedMoves m ++ [Extend r, Main.Rest]
possMoves (SS m               _          ) =                    rangedMoves m ++           [Main.Rest]

rangedMoves :: [RMove] -> [RMove]
rangedMoves (Begin p:prev) = generateMoves p
rangedMoves (_      :prev) = rangedMoves prev
rangedMoves _              = []                            


-- returns a list of RMoves range number of halfsteps above & below p
generateMoves :: Pitch -> [RMove]
generateMoves p =
    let genMoves _ _ 0 = []
        genMoves p f n = let m = f p
                         in Begin m : genMoves m f (n-1)
    in Begin p : genMoves p halfStepUp range ++ genMoves p halfStepDown range


end :: RealizationState -> Bool
end (RS scores accumulating) = null accumulating && null (future (head scores))

type Interval = Int
type IntPreference = (Interval, Float)


intPref :: [IntPreference] -> Int -> Float
intPref prefs i = foldr f 0 prefs
    where f (interval, pay) acc = if i == interval then pay + acc else acc

rmoveInterval :: RMove -> RMove -> Maybe Interval
rmoveInterval (Begin p1)  (Begin p2)  = Just $ interval p1 p2
rmoveInterval (Begin p1)  (Extend p2) = Just $ interval p1 p2
rmoveInterval (Extend p1) (Begin p2)  = Just $ interval p1 p2
rmoveInterval _           _           = Nothing

onePlayerPay :: [RMove] -> [[RMove]] -> [IntPreference] -> Float
onePlayerPay [] _ _ = 0
onePlayerPay _ [] _ = 0
onePlayerPay (me:rs) others ps = foldr f 0 others + onePlayerPay rs others ps
    where f (m:ms) acc = case rmoveInterval me m of
                            Nothing -> acc
                            Just a  -> acc + intPref ps (abs a)

pay :: [IntPreference] -> RealizationState -> Payoff
pay prefs rs = ByPlayer $ p [] (scores rs) prefs
    where p _      []         _     = []
          p before (me:after) prefs = 
              onePlayerPay (realization me) (map realization (before ++ after)) prefs : 
              p (me:before) after prefs

-- Game instance
instance Game Improvise where
  type TreeType Improvise = Discrete
  type Move  Improvise = RMove
  type State Improvise = RealizationState
  gameTree _ = stateTreeD who end markable registerMove (pay samplePrefs) start

samplePrefs = [(4 , 4.0)]

main = evalGame Improvise guessPlayers (run >> printSummaryOfGame 1)
   where run = step >>= maybe run (\p -> printGame >> playMusic >>return p)



-- Players
guessPlayers :: [Hagl.Player Improvise]
guessPlayers = ["A" ::: return (Begin (C, 4)), 
                "B" ::: return (Begin (E, 4))]

-- Printing
printGame :: GameM m Improvise => m ()
printGame = gameState >>= liftIO . putStrLn . show

-- Music generation
playMusic :: (GameM m Improvise, Show (Move Improvise)) => m ()
playMusic = do
    (mss, _) <- liftM (forGame 1) summaries
    liftIO $ Euterpea.play $ rsToMusic (getRS mss)
    return ()


--TODO MAKE SURE ALL LISTS ARE IN CORRECT ORDER -- MAY NEED TO REVERSE

getRS :: MoveSummary (Move Improvise) -> RealizationState
getRS mss = RS (map (\player -> SS (reverse (everyTurn player)) []) (everyPlayer mss)) []

rsToMusic :: RealizationState -> Music Pitch
rsToMusic (RS players _) = foldr (:=:) (Prim (Euterpea.Rest 0)) (map ssToMusic players) 


ssToMusic :: SingularScore -> Music Pitch
ssToMusic (SS realization future) = 
    let condenseMove ((Main.Rest, x):l)   Main.Rest       = (Main.Rest, x + 1):l
        condenseMove ((Begin p1, x):l)   (Extend p2)      = 
            if p1 == p2
            then (Begin p1, x+1):l
            else error "Extend must extend same pitch as most recent pitch"
        condenseMove l mv                                 = (mv,1):l
        condensed                                         = foldl condenseMove [] realization
        condensedToMusic (Main.Rest, d)                   = Prim (Euterpea.Rest (d*baseDur))
        condensedToMusic (Begin p, d)                     = Prim (Note (d*baseDur) p) 
        musicMoves                                        = map condensedToMusic condensed
    in  foldr (:+:) (Prim (Euterpea.Rest 0)) musicMoves


-- | String representation of a move summary.
showMoveSummary :: (Game g, Show (Move g)) =>
  ByPlayer (Hagl.Player g) -> MoveSummary (Move g) -> String
showMoveSummary ps mss = (unlines . map row)
                         (zip (everyPlayer ps) (map everyTurn (everyPlayer mss)))
  where row (p,ms) = "  " ++ show p ++ " moves: " ++ showSeq (reverse (map show ms))
