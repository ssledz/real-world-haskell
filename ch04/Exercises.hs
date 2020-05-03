import Data.Char

-- ch04/84
-- Working with Lists

-- Write your own “safe” definitions of the standard partial list functions, but make
-- sure they never fail. As a hint, you might want to consider using the following types
--
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just $ head xs

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail xs = Just $ tail xs


safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs


safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just $ init xs


-- Write a function splitWith that acts similarly to words but takes a predicate and a
-- list of any type, and then splits its input list on every element for which the predicate
-- returns False :

--  splitWith (\x->x /= ' ') xs == words xs

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f xs = foldr g [[]] xs
    where g y acc@(ys:yys) = if f y then (y:ys):yys else []:acc


-- Using the command framework from the earlier section “A Simple Command-Line
-- Framework” on page 71, write a program that prints the first word of each line of
-- its input.

-- file: FirstWord.hs

-- Write a program that transposes the text in a file. For instance, it should convert
-- "hello\nworld\n" to "hw\neo\nlr\nll\nod\n" .

-- file: TransposeText.hs


-- ch04/97
-- How to Think About Loops

-- Use a fold (choosing the appropriate fold will make your code much simpler) to
-- rewrite and improve upon the asInt function from the earlier section“Explicit Re-
-- cursion” on page 85.

-- Your function should behave as follows:
-- ghci> asInt_fold "101"
-- 101
-- ghci> asInt_fold "-31337"
-- -31337
-- ghci> asInt_fold "1798"
-- 1798

-- Extend your function to handle the following kinds of exceptional conditions by
-- calling error :
-- ghci> asInt_fold ""
-- 0
-- ghci> asInt_fold "-"
-- 0
-- ghci> asInt_fold "-3"
-- -3
-- ghci> asInt_fold "2.7"
-- *** Exception: Char.digitToInt: not a digit '.'
-- ghci> asInt_fold "314159265358979323846"
-- 564616105916946374

asInt :: String -> Int
asInt [] = 0
asInt ys@(x:xs) = case x of
                 '-' -> -1 * (foldl f 0 xs)
                 _ -> foldl f 0 ys
    where f :: Int -> Char -> Int
          f acc x = 10 * acc + digitToInt x
            where digitToInt x
                    | isDigit x = ord x - ord '0'
                    | otherwise = error $ "digitToInt: not a digit " ++ (show x)


-- The asInt function uses error , so its callers cannot handle errors. Rewrite
-- the function to fix this problem:

-- type ErrorMessage = String
-- asInt_either :: String -> Ei
-- ghci> asInt_either "33"
-- Right 33
-- ghci> asInt_either "foo"
-- Left "non-digit 'o'"

type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
asInt_either [] = pure 0
asInt_either ys@(x:xs) = case x of
                 '-' -> fmap ((-1)*) (foldl f (pure 0) xs)
                 _ -> foldl f (pure 0) ys
    where f :: Either ErrorMessage Int -> Char -> Either ErrorMessage Int
          f acc x = pure (+) <*> (pure (10*) <*> acc) <*> (digitToInt x)
            where digitToInt x
                    | isDigit x = pure $ ord x - ord '0'
                    | otherwise = Left $ "digitToInt: not a digit " ++ (show x)

-- The Prelude function concat concatenates a list of lists into a single list and has the
-- following type:
-- concat :: [[a]] -> [a]
-- Write your own definition of concat using foldr .

concat' :: [[a]] -> [a]
concat' xs = foldr (++) [] xs

-- Write your own definition of the standard takeWhile function, first using explicit
-- recursion, and then foldr .

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) = if p x then x:(takeWhile' p xs) else []


takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' p  xs = foldr (\x acc -> if p x then x:acc else []) [] xs

-- The Data.List module defines a function, groupBy , which has the following type:
-- groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
-- Use ghci to load the Data.List module and figure out what groupBy does, then
-- write your own implementation using a fold.

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' p xs = foldr f [] xs
    where f x [] = [[x]]
          f x (ys@(y:_):zs) = if p x y then (x:ys):zs else [x]:ys:zs

-- How many of the following Prelude functions can you rewrite using list folds?
-- • any
-- • cycle
-- • words
-- • unlines
-- For those functions where you can use either foldl' or foldr , which is more ap-
-- propriate in each case?

any' :: (a -> Bool) -> [a] -> Bool       -- more appropriate is foldr because of its short circuit nature
any' p xs = foldr f False xs
    where f x acc = if p x then True else acc


words' :: String -> [String]
words' xs = case foldr f ("",[]) xs of
                ("", ys) -> ys
                (acc, ys) -> acc:ys
    where f x (acc, ys) = if isSpace x then ("", acc:ys) else (x:acc, ys)

unlines' :: [String] -> String
unlines' xs = foldr (\x acc -> x ++ '\n':acc) "" xs

