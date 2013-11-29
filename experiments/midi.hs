import Control.Monad
import Euterpea
import Codec.Midi

testFile :: IO (Either String Codec.Midi.Midi)
testFile = importFile "test.MID"


fromEitherMidi :: Either String Codec.Midi.Midi
     -> Music1
fromEitherMidi (Right m) = 
    let (m2, _, _) = fromMidi m
    in m2

iomus = liftM fromEitherMidi testFile

main = iomus >>= play
