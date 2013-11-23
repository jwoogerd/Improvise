{-# LANGUAGE TypeFamilies #-}

module PureHagl.Exec where

-- | List
newtype ByPlayer a = ByPlayer [a] deriving (Eq, Show)
newtype ByTurn a = ByTurn [a] deriving (Eq, Show)
newtype ByGame a = ByGame [a] deriving (Eq, Show)

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
    type State g
    gameTree :: g -> (TreeType g) (Move g)

data Action = Decision PlayerID | Payoff Payoff deriving Eq

-- | Exec 
data GameState g = GameState 
    { game       :: g 
    , state      :: State g
    , location   :: Action
    , players    :: ByPlayer (Player g)
    , transcript :: Transcript (Move g)
    , history    :: History (Move g)
    , numMoves   :: ByPlayer Int
    , gameNumber :: Int }

type Transition g = GameState g -> g

type Exec g = (GameState g, Transition g)

execGame :: Game g => g -> [Player g] -> Transition g -> GameState g
execGame g ps t = initExec g ps

initExec :: Game g => g -> [Player g] -> GameState g
initExec g ps = GameState g (ByPlayer ps) [] (ByGame []) ms 1
    where ms = ByPlayer (replicate (length ps) 0)
