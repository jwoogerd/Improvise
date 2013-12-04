module IO where
import Codec.Midi (Midi)
import Players
import Game
import Hagl
import System.Environment (getArgs)
import Euterpea hiding (Player)
import Payoff
import Conversions
import State



getInteractive :: [String] -> IO (RealizationState, [Player Improvise], Int, RealizationState -> Payoff)
getInteractive args = do
    imported <-  mapM importFile args
    players <- mapM pickPlayer args
    putStrLn "How many half steps up/down should each player be able to improvise?"
    range <- readLn
    pay <- pickPayoff args
    let start = RS (map (musicToSS . fromEitherMidi) imported) []
    return (start, players, range, pay)

fromEitherMidi :: Either String Midi -> Music Pitch
fromEitherMidi (Right m) = let (m2, _, _) = fromMidi m
                            in mMap fst m2


configWithFiles :: [String] -> IO (RealizationState, [Player Improvise], Int, RealizationState -> Payoff)
configWithFiles args = do
    imported <- mapM importFile args
    let start = RS (map (musicToSS . fromEitherMidi) imported) []
        players = [testBest3,testBest3]
        range   = 14
        pay     = intervalPayoff [player1Prefs,player2Prefs]
    return (start, players, range, pay)

config :: IO (RealizationState, [Player Improvise], Int, RealizationState -> Payoff)
config = let start   = RS [mary, justCNotes] []
             players = [testBest3,testBest3]
             range   = 3
             pay     = intervalPayoff [player1Prefs,player2Prefs]
          in return (start, players, range, pay)


