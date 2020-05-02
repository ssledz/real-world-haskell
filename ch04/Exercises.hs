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

asInt :: String -> Int
asInt [] = 0
asInt xs = foldl f 0 xs
    where f :: Int -> Char -> Int
          f acc x = 10 * acc + digitToInt x
            where digitToInt x = ord x - ord '0'








