
module Main where

stripPlus ('+':cs) = cs
stripPlus      cs  = cs

main = do
  ds <- (fmap (read . stripPlus) . lines) <$> readFile "input"

  putStrLn $ "Part 1: " ++ show (sum ds :: Int)

  let trace = scanl (+) 0 (cycle ds)
      reps = [ f | i <- [0..], let f = trace !! i, f `elem` take i trace ]
  putStrLn $ "Part 2: " ++ show (head reps :: Int)

