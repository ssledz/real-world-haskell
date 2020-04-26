-- Write a function that computes the number of elements in a list. To test it, ensure
-- that it gives the same answers as the standard length function.

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

-- Write a function that computes the mean of a list, i.e., the sum of all elements in
-- the list divided by its length. (You may need to use the fromIntegral function to
-- convert the length of the list from an integer into a floating-point number.)

mean ::  [Double] -> Double
mean xs = mean' xs (fromIntegral 0, 0)
  where mean' :: [Double] -> (Double, Int) -> Double
        mean' [] (_, 0) = 0
        mean' [] (sum, cnt) = sum / (fromIntegral cnt)
        mean' (y:ys) (sum, cnt) = mean' ys (sum + y, cnt + 1)

-- Turn a list into a palindrome; i.e., it should read the same both backward and
-- forward. For example, given the list [1,2,3] , your function should return
-- [1,2,3,3,2,1] .

palindrome :: [a] -> [a]
palindrome xs = xs ++ (revers xs [])
    where revers [] acc = acc
          revers (y:ys) acc = revers ys (y:acc)

-- Write a function that determines whether its input list is a palindrome.

ispalindrome :: Eq a => [a] -> Bool
ispalindrome xs = let l = length xs
                      ys = take (div l 2) xs
                  in xs == palindrome ys

-- Create a function that sorts a list of lists based on the length of each sublist. (You
-- may want to look at the sortBy function from the Data.List module.)

sort :: [[a]] -> [[a]]
sort xs = mergsort xs
    where mergsort :: [[a]] -> [[a]]
          mergsort [x] = [x]
          mergsort [x, y] = if (length x) < (length y) then [x, y] else [y, x]
          mergsort xs = let i = div (length xs) 2
                            l = mergsort (take i xs)
                            r = mergsort (drop i xs)
                        in merge l r where
                            merge :: [[a]] -> [[a]] -> [[a]]
                            merge xs [] = xs
                            merge [] ys = ys
                            merge l@(x:xs) r@(y:ys) = if (length x) < (length y) then x:(merge xs r) else y:(merge l ys)

-- Define a function that joins a list of lists together using a separator value:
-- ghci> intersperse ',' ["foo","bar","baz","quux"]
-- "foo,bar,baz,quux"

intersperse :: a -> [[a]] -> [a]
intersperse sep [] = []
intersperse sep [x] = x
intersperse sep [x, y] = x ++ [sep] ++ y
intersperse sep (x:xs) = x ++ [sep] ++ (intersperse sep xs)

-- Using the binary tree type that we defined earlier in this chapter, write a function
-- that will determine the height of the tree. The height is the largest number of hops
-- from the root to an Empty . For example, the tree Empty has height zero; Node "x"
-- Empty Empty has height one; Node "x" Empty (Node "y" Empty Empty) has height
-- two; and so on.

data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

height :: Tree a -> Int
height Empty = 0
height (Node _ Empty Empty) = 1
height (Node _ l r) = 1 + max (height l) (height r)

-- Consider three two-dimensional points, a, b, and c. If we look at the angle formed
-- by the line segment from a to b and the line segment from b to c, it turns left, turns
-- right, or forms a straight line. Define a Direction data type that lets you represent
-- these possibilities.

data Point2d = Point2d Double Double

data Direction = TurnLeft | TurnRight | StraightLine deriving Show

-- Write a function that calculates the turn made by three two-dimensional points
-- and returns a Direction .

turn :: Point2d -> Point2d -> Point2d -> Direction
turn a b c = TurnLeft
-- TODO

-- Define a function that takes a list of two-dimensional points and computes the
-- direction of each successive triple. Given a list of points [a,b,c,d,e] , it should
-- begin by computing the turn made by [a,b,c] , then the turn made by [b,c,d] ,
-- then [c,d,e] . Your function should return a list of Direction .

-- TODO

-- Using the code from the preceding three exercises, implement Graham’s scan al-
-- gorithm for the convex hull of a set of 2D points. You can find good description
-- of what a convex hull (http://en.wikipedia.org/wiki/Convex_hull) is, and how the
-- Graham scan algorithm (http://en.wikipedia.org/wiki/Graham_scan) should work,
-- on Wikipedia (http://en.wikipedia.org/).

-- TODO

