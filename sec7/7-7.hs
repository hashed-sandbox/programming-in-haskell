unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

chop8 :: [a] -> [[a]]
chop8 = unfold null (take 8) (drop 8)

_map :: ([a] -> b) -> [a] -> [b]
_map f = unfold null f tail

_iterate :: (a -> a) -> a -> [a]
_iterate f = unfold (const False) id f
