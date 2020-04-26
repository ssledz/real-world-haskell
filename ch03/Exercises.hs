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
