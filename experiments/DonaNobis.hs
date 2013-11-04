--NOTE: need to run this in euterpea examples directory

import Euterpea
import Euterpea.Examples.Interlude


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


convert::(PitchClass, Rational,Octave) -> Music Pitch
convert (pc, r, o) = Prim (Note r (pc, 5+o))

notes::Music Pitch
notes = foldr (:+:) (Prim (Rest 0)) (map convert x)

round = notes :=: (delayM 6 notes)
