import Data.Char (toUpper)

--[1,2,3,4,5,6]
oneToSix :: [Int]
oneToSix = [x | x <- [1 .. 6]]

--[10,20,30,40,50,60]
tenToSixty :: [Int]
tenToSixty = [x * 10 | x <- [1 .. 6]]

--[(1,1),(2,2),(3,3),(4,4)]
listOfTupleOfSame :: [(Int, Int)]
listOfTupleOfSame = [(x, x) | x <- [1 .. 4]]

--[(1,2),(2,3),(3,4),(4,5)]
listOfNPlusOneTuple :: [(Int, Int)]
listOfNPlusOneTuple = [(x, x + 1) | x <- [1 .. 4]]

--myConstFunc = [(1, 1), (2, 1), (3, 1), (4, 1), (5, 1)]
myConstFunc :: [(Int, Int)]
myConstFunc = [(x, 1) | x <- [1 .. 5]]

isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0

doubleAll :: [Integer] -> [Integer]
doubleAll xs = [x * 2 | x <- xs]

-- that import is needed for this one
capitalise :: String -> String
capitalise str = [toUpper x | x <- str]

sigma :: Int
sigma = sum [x^2 | x <- [1 .. 100]]

sigma' :: Int -> Int
sigma' n = sum [x^2 | x <- [1 .. n]]

-- important
matches :: Integer -> [Integer] -> [Integer]
matches x xs = [x | (x', i) <- zip xs [0 ..], x == x']

grid :: Int -> Int -> [(Int, Int)]
grid x y = [(x, y) | x <- [0 .. x], y <- [0 .. y]]

square :: Int -> [(Int, Int)]
square n = [(x, y) | x <- [0 .. n], y <- [0 .. n], x /= y]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [0 .. n], y <- [0 .. n], z <- [0 .. n], x^2 + y^2 == z^2]

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x|x<-[1..n], sum(init(factors n)) ==x]
