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









