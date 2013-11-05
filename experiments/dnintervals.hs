import Euterpea


donanobis = [(G,1/8,0),(D,1/8,0),(B,1/2,0),
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

type Interval = Int

dndurs :: [(PitchClass, Dur, Octave)] -> [Dur]
dndurs [] = []
dndurs ((pc,d,o):rest) = d:dndurs rest

dnpitchs :: [(PitchClass, Dur, Octave)] -> [Pitch]
dnpitchs [] = []
dnpitchs ((pc, d, o):rest) = (pc, (o + 5)): dnpitchs rest


hNote :: Dur -> Pitch -> Interval -> Music Pitch 
hNote d p i = note d p :=: note d (trans i p)

hList :: [Dur] -> [Pitch] -> [Interval] -> Music Pitch
hList (d:ds) (p:ps) (i:is) = hNote d p i :+: hList ds ps is
hList _ _ _ = rest 0


meldn :: [Interval] -> Music Pitch 
meldn is = hList (dndurs donanobis) (dnpitchs donanobis) is

nicedn = meldn (repeat (-3))
baddn = meldn (cycle [3, 4, -2, 2])
