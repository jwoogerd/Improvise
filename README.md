###music-improv

Modeling and implementing interactive music composition as a two-player
cooperative game using Hagl.

``` haskell
    data RMove = Begin Pitch
               | Extend Pitch 
               | Rest deriving (Show, Eq)

    data SingularScore = SS { realization :: [RMove]
                            , future      :: [RMove] } deriving Show

    data RealizationState = RS { scores       :: [SingularScore]
                               , accumulating :: [RMove] } deriving Show
```
 
