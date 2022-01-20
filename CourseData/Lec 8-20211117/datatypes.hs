-- discussion on datatypes.

data Season = Spring | Summer | Autumn | Winter
            deriving (Eq, Show)

next Spring = Summer
next Summer = Autumn
next Autumn = Winter
next Winter = Spring

{-              
data MyBool = MyFalse | MyTrue
            deriving (Eq, Show)
                     
instance Eq MyBool where
    MyFalse == MyFalse = True
    MyTrue == MyTrue = True
    _ == _ = False

instance Show MyBool where
    show MyTrue = "True"
    show MyFalse = "False"
-}

data BinT a = EmptyT | Node a (BinT a) (BinT a)
            deriving (Eq, Show)

isEmpty EmptyT  = True
isEmpty _ = False 

subT EmptyT = error "empty bin tree"
subT (Node a l r) = (l,r)

leftT a =
    case a of
      EmptyT -> error "empty bin tree"
      Node _ l _ -> l
{-
Proof: 
Base: 
I.H:
I.S: 
-}
rightT a =
    case a of
      EmptyT -> error "empty bin tree"
      Node _ _ r -> r 
              
