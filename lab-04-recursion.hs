sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

exponention :: Int -> Int -> Int
exponention _ 0 = 1
exponention n e = exponention n (e - 1)

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci i = fibonacci (i - 1) + fibonacci (i - 2)

myInit :: [a] -> [a]
myInit [_] = []
myInit (x : xs) = x : myInit xs

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x : xs) = x == myAnd xs

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x : xs) = x ++ myConcat xs

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate i a = myReplicate (i - 1) a

myNth :: Ord a => [a] -> Int -> a
myNth (x : xs) 0 = x
myNth (x : xs) n = myNth (xs) (n -1)

myElm :: Ord a => [a] -> a -> Bool
myElm [] _ = False
myElm (x : xs) a = a == x || myElm xs a

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (i : is) = i + mySum is

myTake :: Int -> [b] -> [b]
myTake 0 _ = []
myTake _ [] = []
myTake i (x : xs) = x : myTake (i - 1) xs

myLast :: [a] -> a
myLast [x] = x
myLast (_ : xs) = myLast xs

myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (i : is) = i * myProduct is

doubleAll :: Num a => [a] -> [a]
doubleAll [] = []
doubleAll (i : is) = i * 2 : doubleAll is

isort :: Ord a => a -> [a] -> [a]
isort x [] = [x]
isort x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : isort x ys

insertSort :: Ord a => [a] -> [a]
insertSort [] = []
insertSort (x : xs) = isort x (insertSort xs)

mapLookup :: [(Integer, String)] -> [Integer] -> [String]
mapLookup _ [] = []
mapLookup ns (i : is) =
  locate i : mapLookup ns is
  where
    locate i =
      let go i ns =
            case ns of
              [] -> undefined
              ((i', n) : xs) ->
                if i == i'
                  then n
                  else go i xs
       in go i ns
