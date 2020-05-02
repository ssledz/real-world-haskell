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

firstWord :: String -> String
firstWord line = headOpt $ words line
    where headOpt [] = []
          headOpt xs = head xs

firstWords :: [String] -> [String]
firstWords xs = map firstWord xs

myFunction txt = unlines $ firstWords (lines txt)

