logB base n = log (fromIntegral n) / log (fromIntegral base)
logRemainder n x | (rem x n) == 0 = logRemainder n (div x n)
                 | otherwise = x
data Pair = Pair Int deriving Show
cons a b = Pair $ (2 ^ a) * (3 ^ b)
car (Pair representation) = round $ logB 2 $ logRemainder 3 representation
cdr (Pair representation) = round $ logB 3 $ logRemainder 2 representation
