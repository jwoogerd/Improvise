{-# LANGUAGE TypeFamilies, RankNTypes #-}

module PureHagl.Exec where

import PureHagl.Lists
-- | Strategy
type Strategy g = GameState g -> Move g

-- | Player 
type PlayerID = Int 
type Name = String 
data Player g = Player Name (Strategy g)

-- | Payoff
type Payoff = ByPlayer Float

-- | History
type MoveEvent mv = (Maybe PlayerID, mv)
type Transcript mv = [MoveEvent mv]
type MoveSummary mv = ByPlayer (ByTurn mv)
type Summary mv = (MoveSummary mv, Maybe Payoff)
type History mv = ByGame (Transcript mv, Summary mv)

-- | Game 

class Game g where
    type TreeType g :: * -> * 
    type Move g
    gameTree :: g -> (TreeType g) (Move g)

data Node = Decision PlayerID | Payoff Payoff deriving Eq

-- | Exec 
data GameState g = GameState 
    { game       :: Game g => g 
    , location   :: Node
    , players    :: [Player g]
    , transcript :: Transcript (Move g)
    , history    :: History (Move g)
    , numMoves   :: ByPlayer Int
    , transition :: Transition g
    , gameNumber :: Int }

type Transition g = GameState g -> g

execGame :: Game g => g -> [Player g] -> Transition g -> Run g -> 
    GameState g
execGame g ps t r  = initGame g ps t 1

initGame :: Game g => g -> [Player g] -> Transition g -> Int ->  GameState g
initGame g ps t i = GameState g (Decision 1) ps [] (ByGame []) ms t i
    where ms = ByPlayer (replicate (length ps) 0)

--
type Run g = Game g => GameState g -> GameState g

-- one step in the game
step :: Run g 
step gs =
    let t          = transition gs
        gameNum    = gameNumber gs + 1
        gameState  = case location gs of 
            Payoff _   -> initGame (t gs) (players gs) t gameNum
            Decision _ -> gs
    in gameState --TODO ACTUALLY MOVE FORWARD

runN :: Run g -> Int -> Run g
runN toRun i = 
    case i of
        1 -> toRun
        _ -> toRun . (runN toRun (i-1))

-- contract: location (finish gs)  :: Payoff Payoff
finish :: Run g
finish gs =
    case location gs of
        Payoff _   -> gs
        Decision _ -> finish (step gs)
        
