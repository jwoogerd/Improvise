import Codec.Midi (Midi)
import Euterpea
import System.Environment (getArgs)
    

fromEitherMidi :: Either String Midi -> Music Pitch
fromEitherMidi (Right m) = let (m2, _, _) = fromMidi m
                            in mMap fst m2

main = do args <- getArgs
          files <- mapM importFile args
          play $ foldr ((:=:) . fromEitherMidi) (Prim (Rest 0)) files
          --play $ Modify (Transpose 0) (fromEitherMidi file)
