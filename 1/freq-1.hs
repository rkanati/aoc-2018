
module Main where

stripPlus ('+':cs) = cs
stripPlus      cs  = cs

main = do
  ls <- lines <$> readFile "input"
  let ls' = fmap stripPlus ls
      result = sum $ fmap read $ ls'
  putStrLn $ show (result :: Int)

