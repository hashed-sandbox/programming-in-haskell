(\/) :: Bool -> Bool -> Bool

-- (1)
{-
True  \/ True  = True
True  \/ False = True
False \/ True  = True
False \/ False = False
-}

-- (2)
{-
False \/ False = False
_     \/ _     = True
-}

-- (3)
{-
True  \/ _ = True
False \/ b = b
-}

-- (4)
b \/ c | b == c    = b
       | otherwise = True
