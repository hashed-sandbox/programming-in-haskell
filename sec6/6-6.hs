_sum :: Num a => [a] -> a
_sum []     = 0
_sum (n:ns) = n + _sum ns

_take :: Int -> [a] -> [a]
_take _ []     = []
_take 0 _      = []
_take n (x:xs) = x : _take (n-1) xs

_last :: [a] -> a
_last (x:xs) | null xs   = x
             | otherwise = _last xs
