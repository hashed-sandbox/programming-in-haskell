safetail :: [a] -> [a]

-- (1)
safetail xs = if null xs then [] else tail xs

-- (2)
{-
safetail xs | null xs = []
            | otherwise = tail xs
-}

-- (3)
{-
safetail [] = []
safetail xs = tail xs
-}
