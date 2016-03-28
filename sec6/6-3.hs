_and :: [Bool] -> Bool
_and []     = True
_and (b:bs) = b && _and bs

_concat :: [[a]] -> [a]
_concat [[]]   = []
_concat (l:ls) = l ++ concat ls

_replicate :: Int -> a -> [a]
_replicate 0 x = []
_replicate n x = x : replicate (n-1) x

(!!!) :: [a] -> Int -> a
(x:xs) !!! 0 = x
(x:xs) !!! n = xs !!! (n-1)

_elem :: Eq a => a -> [a] -> Bool
_elem v [] = False
_elem v (x:xs) | v == x    = True
               | otherwise = _elem v xs
