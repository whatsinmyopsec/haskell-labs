halve :: [a] -> ([a], [a])
halve xs = (take half xs, drop half xs)
  where
    half = length xs `div` 2

-- Try using guards

third :: [a] -> a
third xs = head $ drop 2 xs

safetail :: [a] -> [a]
safetail xs
  | null xs = []
  | otherwise = tail xs

orOperator :: Bool -> Bool -> Bool
False `orOperator` False = False
_ `orOperator` _ = True

lucky :: Integral a => a -> String
lucky 7 = "you were lucky"
lucky 9 = "not your day"
lucky _ = "go away"

firstTuple :: (a, b, c) -> a
firstTuple (a, _, _) = a

secondTuple :: (a, b, c) -> b
secondTuple (_, b, _) = b

thirdTuple :: (a, b, c) -> c
thirdTuple (_, _, c) = c

luhnDouble :: Int -> Int
luhnDouble x = if (x * 2 > 9) then x * 2 -9 else x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (a' + b + c' + d) `mod` 10 == 0
  where
    a' = luhnDouble a
    c' = luhnDouble c

luhnCheck :: Int -> Int -> Int -> Int
luhnCheck a b c = 10 - (luhnSum `mod` 10)
  where
    luhnSum = luhnDouble a + b + luhnDouble c
