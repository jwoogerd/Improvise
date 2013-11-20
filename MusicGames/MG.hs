{-# LANGUAGE FlexibleContexts, TypeFamilies #-}


import Control.Monad.Trans (liftIO)
import Control.Monad (liftM,liftM2,unless)
import Control.Monad.State
import Hagl.History
import Hagl
import Euterpea
import Translations
import Debug.Trace
import Codec.Midi
import Data.Ratio

-- test data and constants (probably to be removed later)
octv :: Octave
octv = 5

dummyPayoff :: Float
dummyPayoff = 1.0

baseDur :: Dur
baseDur = 1/8

range = 4

player1 :: SingularScore
player1 = SS [] [Begin (C,4), Extend (C, 4), Begin (D,4)]
player2 :: SingularScore
player2 = SS [] [Begin (D,4), Extend (D, 4), Begin (C, 4)]

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

registerMove :: RealizationState -> RMove -> RealizationState
registerMove rs mv = 
    if length (accumulating newRS) == length (scores newRS)
    then progress newRS
    else newRS
        where newRS = RS (scores rs) (mv: accumulating rs)
              progress rs = let step p mv  = SS (mv: realization p) (tail $ future p)
                                newPlayers = zipWith step (scores rs) (reverse $ accumulating rs)
                            in RS newPlayers []

possMoves :: SingularScore -> [RMove]
possMoves (SS _               []         ) = []
possMoves (SS m@(Begin r:rs) (Begin f:fs)) = Main.Rest: Extend r: rangedMoves m ++ generateMoves f 
possMoves (SS m              (Begin f:fs)) = Main.Rest:           rangedMoves m ++ generateMoves f
possMoves (SS m@(Begin r:rs)  _          ) = Main.Rest: Extend r: rangedMoves m
possMoves (SS m               _          ) = Main.Rest:           rangedMoves m

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
rmoveInterval (Extend p1) (Extend p2) = Just $ interval p1 p2
rmoveInterval _           _           = Nothing

onePlayerPay :: [RMove] -> [[RMove]] -> [IntPreference] -> Float
onePlayerPay [] _ _ = 0
onePlayerPay _ [] _ = 0
onePlayerPay (me:rs) others ps = foldr f 0 others + onePlayerPay rs (map tail others) ps
    where f (m:ms) acc = case rmoveInterval me m of
                            Nothing -> acc
                            Just a  -> acc + intPref ps a

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

--samplePrefs = [(4 , 4.0), (1, 1.0), (2, 10.0), (6, 5), (3, 2), (0, 3)]
--samplePrefs = [(1 , 1.0), (2, 2.0), (3, 3.0), (4, 4), (5, 5), (6, 6)]
samplePrefs = [(1, -1), (2, -1), (3, 0), (4 , 5), (5, 0), (6, -1), (7, 5), (8, -1), (9, -1), (10, -1), (11, -1), (12, 3)]

main = evalGame Improvise guessPlayers (run >> printSummary)
   where run = step >>= maybe run (\p -> printGame >> playMusic >>return p)



-- Players
guessPlayers :: [Hagl.Player Improvise]
guessPlayers = [testPlayScore, testMinimax]

testPeriodic :: Hagl.Player Improvise
testPeriodic = "Miss Periodic" ::: periodic [Begin (C, 4), Begin (D, 4), Begin (E, 4), Begin (F, 4)]

testMinimax :: Hagl.Player Improvise
testMinimax  = "Mr. Minimax" ::: minimax

testPlayScore :: Hagl.Player Improvise
testPlayScore = "Mr. Score" :::
    do let ss = map future $ scores start
       n  <- my numMoves
       id <- myPlayerID
       return ((ss !! (id-1)) !! n)

-- Printing
printGame :: GameM m Improvise => m ()
printGame = gameState >>= liftIO . putStrLn . show

-- Music generation
playMusic :: (GameM m Improvise, Show (Move Improvise)) => m ()
playMusic = do
    (mss, _) <- liftM (forGame 1) summaries
    liftIO $ Euterpea.play $ rsToMusic (getRS mss)
    return ()


-- convert from our representation to Euterpea Music
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
    in  foldr (:+:) (Prim (Euterpea.Rest 0)) (reverse musicMoves)

-- convert from Euterpea Music to our representation
okDur :: Dur -> Bool
okDur d = elem (denominator d) [1,2,4,8]

genRMoves :: RMove -> Dur -> [RMove]
genRMoves r d = if okDur d then take (floor (d * (1/baseDur))) (repeat r) else error "invalid Duration"

musicToRMoves :: Music Pitch -> [RMove]
musicToRMoves (Prim (Note d p))          = Begin p : (genRMoves (Extend p) (d - baseDur))
musicToRMoves (Prim (Euterpea.Rest d  )) = genRMoves  Main.Rest d
musicToRMoves (m1 :+: m2)                = (musicToRMoves m1) ++ (musicToRMoves m2)
musicToRMoves (m1 :=: m2)                = error "Cannot parse overlay"
musicToRMoves (Modify c m1)              = trace ("Warning: discarding " ++ show c) musicToRMoves m1
  
musicToSS :: Music Pitch -> SingularScore
musicToSS m = SS [] (reverse (musicToRMoves m))



-- | String representation of a move summary.
showMoveSummary :: (Game g, Show (Move g)) =>
  ByPlayer (Hagl.Player g) -> MoveSummary (Move g) -> String
showMoveSummary ps mss = (unlines . map row)
                         (zip (everyPlayer ps) (map everyTurn (everyPlayer mss)))
  where row (p,ms) = "  " ++ show p ++ " moves: " ++ showSeq (reverse (map show ms))


-- starting to import music
testFile :: IO (Either String Codec.Midi.Midi)
testFile = importFile "test.MID"


fromEitherMidi :: Either String Codec.Midi.Midi
     -> Music Pitch
fromEitherMidi (Right m) = 
    let (m2, _, _) = fromMidi m
    in mMap fst m2

iomus = liftM fromEitherMidi testFile

fromio = iomus >>= Euterpea.play


