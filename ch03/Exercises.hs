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





