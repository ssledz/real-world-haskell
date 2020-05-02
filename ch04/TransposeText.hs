import System.Environment (getArgs)

main = run myFunction
    where run f = do
            args <- getArgs
            case args of
              [input, output] -> interactWith f input output
              _ -> putStrLn "error: Two arguments needed!"


interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith f inputFile outputFile  = do
    input <- readFile inputFile
    writeFile outputFile (f input)

transpose :: [String] -> [String]
transpose xs = case foldl f (Nothing, []) xs of
                 (Just y, ys) -> ys ++ [y]
                 (_, ys) -> ys
    where f (Nothing, ys) x = (Just x, ys)
          f (Just x, zs) y = let tr = zipWith (\a b -> [a, b]) x y
                             in (Nothing, zs ++ tr)

myFunction :: String -> String
myFunction txt = unlines $ transpose (lines txt)

