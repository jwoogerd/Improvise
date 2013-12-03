{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
import PureHagl
import Euterpea
import Data.List    (union)

import Translations

import Codec.Midi
import Debug.Trace  (trace)
import Data.Ratio   (denominator)
import Control.Monad (liftM)


baseDur :: Dur
baseDur = 1/8

range = 2

player1 :: SingularScore
player1 = SS [] [Begin (A, 4), Extend (A, 4), Begin (G,4), Extend (G, 4),
                 Begin (F, 4), Extend (F, 4), Begin (G, 4), Extend (G, 4),
                 Begin (A, 4), Extend (A, 4), Begin (A, 4), Extend (A, 4),
                 Begin (A, 4), Extend (A, 4), Extend (A, 4), Extend (A, 4)]
player2 :: SingularScore
player2 = SS [] (replicate 96 (Begin (C, 4)))

-- DonnaNobis
x :: [(PitchClass,Rational,Octave)]
x = [(G,1/8,0),(D,1/8,0),(B,1/2,0),
     (A,1/8,0),(D,1/8,0),(C,1/2,1),
     (B,1/4,0),(A,1/4,0),(G,1/4,0),
     (G,1/4,0),(Fs,1/2,0),
     (E,1/4,1),(D,1/8,1),(C,1/8,1),(B,1/8,0),(A,1/8,0),
     (D,3/8,1),(C,1/8,1),(B,1/4,0),
     (B,1/8,0),(A,1/8,0),(G,1/4,0),(Fs,1/4,0),(G,3/4,0),
     (D,3/4,1),
     (D,3/4,1),
     (D,1/4,1),(C,1/4,1),(B,1/4,0),
     (B,1/4,0),(A,1/2,0),
     (E,1/4,1),(E,1/2,1),
     (D,1/4,1),(D,1/2,1),
     (D,1/8,1),(C,1/8,1),(B,1/4,0),(A,1/4,0),
     (G,3/4,0)]

convert :: (PitchClass, Rational, Octave) -> Music Pitch
convert (pc, r, o) = Prim (Note r (pc, 4+o))

notes :: Music Pitch
notes = foldr (:+:) (Prim (Euterpea.Rest 0)) (map convert x)

donnaPlayer :: SingularScore
donnaPlayer = musicToSS notes 

--samplePrefs = [(1, -1), (2, -1), (3, 0), (4 , 5), (5, 0), (6, -1), 
--               (7, 5), (8, -1), (9, -1), (10, -1), (11, -1), (12, 3)]

--player1Prefs = [(1, -1), (2, -1), (3, 1)]
player1Prefs = []
--player2Prefs = [(4, 1), (5, 1), (6, -1)]
player2Prefs = [(3,5),(4,-1)]

--
-- * Game representation
--

-- | A trivial data type for Improvise.
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


-- | The initial state.
start :: RealizationState
--start = RS [player1, player2] []
start = RS [donnaPlayer, player2] []

-- | The player whose turn it is.
who :: RealizationState -> PlayerID
who rs = length (accumulating rs) + 1

-- | Get a list of the available moves for a player, which is derived from her 
-- score and past realization.
playable :: RealizationState -> [RMove]
playable rs = possMoves $ scores rs !! length (accumulating rs)

registerMove :: RealizationState -> RMove -> RealizationState
registerMove rs mv = if length (accumulating newRS) == length (scores newRS)
                     then progress newRS
                     else newRS
    where newRS       = RS (scores rs) (mv: accumulating rs)
          progress rs = let step p mv  = SS (mv: realization p) 
                                            (tail $ future p)
                            newPlayers = zipWith step (scores rs) 
                                         (reverse $ accumulating rs)
                        in RS newPlayers []

possMoves :: SingularScore -> [RMove]
possMoves (SS _               []         ) = []
possMoves (SS m@(Begin r:rs) (Begin f:fs)) = union (Main.Rest: Extend r: rangedMoves m) (generateMoves f) 
possMoves (SS m              (Begin f:fs)) = union (Main.Rest:           rangedMoves m) (generateMoves f)
possMoves (SS m@(Begin r:rs)  _          ) =        Main.Rest: Extend r: rangedMoves m
possMoves (SS m@(Extend r:rs) _          ) =        Main.Rest: Extend r: rangedMoves m
possMoves (SS m               _          ) =        Main.Rest:           rangedMoves m

rangedMoves :: [RMove] -> [RMove]
rangedMoves (Begin p:prev) = generateMoves p
rangedMoves (_      :prev) = rangedMoves prev
rangedMoves _              = []                            

generateMoves :: Pitch -> [RMove]
generateMoves p =
    let genMoves _ _ 0 = []
        genMoves p f n = let m = f p
                         in Begin m: genMoves m f (n-1)
    in Begin p: genMoves p halfStepUp range ++ genMoves p halfStepDown range


-- | True if the game is over.
end :: RealizationState -> Bool
end (RS scores accumulating) = null accumulating && null (future (head scores))

--
-- * Payoff generation
--

-- TODO: add more payoff schemes and move to different module? 

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
onePlayerPay (me:rs) others ps = 
    foldr f 0 others + onePlayerPay rs (map tail others) ps
    where f (m:ms) acc = case rmoveInterval me m of
                            Nothing -> acc
                            Just a  -> acc + intPref ps a

pay :: [[IntPreference]] -> RealizationState -> Payoff
pay prefs rs = ByPlayer $ p [] (scores rs) prefs
    where p _      []         _            = []
          p before (me:after) (myPrefs:ps) = 
              onePlayerPay (realization me) (map realization (before ++ after)) myPrefs: 
                p (me:before) after ps


instance Game Improvise where
    type TreeType Improvise = Discrete
    type Move Improvise = RMove
    type State Improvise = RealizationState
    gameTree _ = stateTreeD who end playable registerMove 
                 (pay [player1Prefs, player2Prefs]) start 


music = execGame Improvise scoreVDepth game once


main = playMusic music


--
-- * Players
--

-- | A player who never deviates from the score.
mrScore :: PureHagl.Player Improvise
mrScore = PureHagl.Player "Mr. Score" justTheScore

--
-- * Strategies
--

-- | A player playing this strategy will stick to the score, i.e. never
-- improvise.
justTheScore :: PureHagl.Strategy Improvise
justTheScore gs = let ss = map future $ scores start
                      n  = my numMoves gs
                      id = myPlayerID gs
                  in (ss !! (id-1)) !! n

-- | Improv game specific strategy of depth limited minimax
mmlStrat :: PureHagl.Strategy Improvise
mmlStrat = minimaxLimited 4 (pay [player1Prefs, player2Prefs])

--
-- * Player Sets
--


-- | Two using depth limited
depthVDepth :: [PureHagl.Player Improvise]
depthVDepth = [PureHagl.Player "me" mmlStrat, PureHagl.Player "you" mmlStrat]

-- | First player plays score, second improvises
scoreVDepth :: [PureHagl.Player Improvise]
scoreVDepth = [PureHagl.Player "me" justTheScore, PureHagl.Player "you" mmlStrat]

-- | Two musicians playing minimax against each other.
miniVsMini :: [PureHagl.Player Improvise]
miniVsMini = [PureHagl.Player "me" minimax, PureHagl.Player "you" minimax]

-- | Two musicians just playing the score.
scoreVsScore :: [PureHagl.Player Improvise]
scoreVsScore = [PureHagl.Player "Miss Score" justTheScore, mrScore]

-- | One musician plays minimax, the other just play the score.
miniVsScore :: [PureHagl.Player Improvise]
miniVsScore = [PureHagl.Player "Miss Mini" minimax, mrScore]


-- 
-- * Printing
--

{-
-- Printing
printGame :: GameM m Improvise => m ()
printGame = gameState >>= liftIO . putStrLn . show

-}
-- Music generation

playMusic :: (GameState Improvise) => IO ()
playMusic gs = Euterpea.play $ rsToMusic (getRS (forGame 1 (moves gs)))
{-
    playMusic gs = if isNewGame gs 
               then if gameNumber gs == 1
                    then Euterpea.play $ rsToMusic (treeState (_location gs))
                    else Euterpea.play $ rsToMusic (getRS (forGame ((gameNumber gs) - 1) (moves gs)) )
               else Euterpea.play $ rsToMusic (treeState (_location gs))
                   -}



-- convert from our representation to Euterpea Music
getRS :: MoveSummary (Move Improvise) -> RealizationState
getRS mss = RS (map 
                    (\player -> SS (reverse (everyTurn player)) []) 
                    (everyPlayer mss)) 
               []
    where btmoves :: [ByTurn (Move Improvise)]
          btmoves = everyPlayer mss
          getMoves :: ByTurn (Move Improvise) -> SingularScore
          getMoves player = SS (reverse (everyTurn player)) []

rsToMusic :: RealizationState -> Music Pitch
rsToMusic (RS players _) = foldr ((:=:) . ssToMusic) 
                                 (Prim (Euterpea.Rest 0)) 
                                 players 


ssToMusic :: SingularScore -> Music Pitch
ssToMusic (SS realization future) = 
    let condenseMove ((Main.Rest, x):l) Main.Rest   = (Main.Rest, x + 1):l
        condenseMove ((Begin p1, x):l)  (Extend p2) = 
            if p1 == p2
            then (Begin p1, x+1):l
            else error "Extend must extend same pitch as most recent pitch"
        condenseMove l mv                    = (mv,1):l
        condensed                            = foldl condenseMove [] 
                                                     (realization ++ future)
        condensedToMusic (Main.Rest, d)      = Prim (Euterpea.Rest (d*baseDur))
        condensedToMusic (Begin p, d)        = Prim (Note (d*baseDur) p) 
        musicMoves                           = map condensedToMusic condensed
    in  foldr (:+:) (Prim (Euterpea.Rest 0)) (reverse musicMoves)

-- convert from Euterpea Music to our representation
okDur :: Dur -> Bool
okDur d = denominator d `elem` [1,2,4,8]

genRMoves :: RMove -> Dur -> [RMove]
genRMoves r d = if okDur d then replicate (floor (d * (1/baseDur))) r else error "invalid Duration"

musicToRMoves :: Music Pitch -> [RMove]
musicToRMoves (Prim (Note d p))          = Begin p: genRMoves (Extend p) (d - baseDur)
musicToRMoves (Prim (Euterpea.Rest d  )) = genRMoves  Main.Rest d
musicToRMoves (m1 :+: m2)                = musicToRMoves m1 ++ musicToRMoves m2
musicToRMoves (m1 :=: m2)                = error "Cannot parse overlay"
musicToRMoves (Modify c m1)              = trace ("Warning: discarding " ++ show c) musicToRMoves m1
  
musicToSS :: Music Pitch -> SingularScore
musicToSS m = SS [] (musicToRMoves m)


{-
-- | String representation of a move summary.
showMoveSummary :: (Game g, Show (Move g)) =>
  ByPlayer (Hagl.Player g) -> MoveSummary (Move g) -> String
showMoveSummary ps mss = 
    (unlines . map row) (zip (everyPlayer ps) (map everyTurn (everyPlayer mss)))
  where row (p,ms) = "  " ++ show p ++ " moves: " ++ 
                     showSeq (reverse (map show ms))

-}
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


