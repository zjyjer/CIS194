strLength :: String -> Int
strLength [] = 0
strLength (_:xs) = let len_rest = strLength xs in
                   len_rest + 1

frob :: String -> Char
frob [] = 'a'
frob str
  | len > 5 = 'x'
  | len < 3 = 'y'
  | otherwise = 'z'
  where
    len = strLength str

sumTo20 :: [Int] -> Int
sumTo20 = go 0
  where go :: Int -> [Int] -> Int
        go acc [] = acc
        go acc (x:xs)
         | acc >= 20 = acc
         | otherwise = go (acc + x) xs

strange :: a -> b
strange = error "impossible"

limited :: a -> a
limited x = x

-- head tail init last and (!!) are partial functions.
doStuff1 :: [Int] -> Int
doStuff1 [] = 0
doStuff1 [_] = 0
doStuff1 xs = head xs + head (tail xs)

doStuff2 :: [Int] -> Int
doStuff2 [] = 0
doStuff2 [_] = 0
doStuff2 (x1:x2:_) = x1 + x2

add1Mul4 :: [Int] -> [Int]
add1Mul4  = map ((* 4) . (+ 1))

negateNumEvens1 :: [Int] -> Int
negateNumEvens1 x = negate (length (filter even x))

negateNumEvens2 :: [Int] -> Int
negateNumEvens2 x = negate $ length $ filter even x

duplicate1 :: [String] -> [String]
duplicate1 = map dup
  where dup x = x ++ x

duplicate2 :: [String] -> [String]
duplicate2 = map (\x -> x ++ x)

foobar :: [Integer] -> Integer
foobar [] = 0
foobar (x:xs)
  | x > 3 = (7 * x + 2) + foobar xs
  | otherwise = foobar xs

foobar' :: [Integer] -> Integer
foobar' = sum . map ((+ 2) . (* 7)) . filter (> 3)
