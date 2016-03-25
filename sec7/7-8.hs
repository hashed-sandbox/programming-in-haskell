import Data.Char

type Bit = Int

----------------------------------------

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

paritify :: [Bit] -> [Bit]
paritify bits = ((sum bits) `mod` 2) : bits

encode :: String -> [Bit]
encode = concat . map (paritify . make8 . int2bin . ord)

----------------------------------------

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

deparitify :: [Bit] -> [Bit]
deparitify bits | ((sum bits) `mod` 2  == 0) = tail bits
                | otherwise                  = error "bad bits\n"

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

decode :: [Bit] -> String
decode = map (chr . bin2int . deparitify) . chop9

----------------------------------------

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode
