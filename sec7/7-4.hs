dec2int :: [Int] -> Int

dec2int ns = foldl (\x y -> 10*x + y) 0 ns

{-
dec2int ns = sum [w * n | (w, n) <- zip weights (reverse ns)]
             where weights = iterate (*10) 1
-}
