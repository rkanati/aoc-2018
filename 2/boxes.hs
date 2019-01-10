
module Main where

import Data.List (findIndex, transpose)

histo :: Eq a => [a] -> [(a, Int)]
histo = go [] where
  go h [] = h
  go h (a:as) = go h' as where
    h' = case findIndex (\(k,_) -> k == a) h of
          Just i ->
            let (_, n) = h !! i
                (head', _:tail') = splitAt i h
            in (a, n + 1) : head' ++ tail'
          Nothing ->
            (a, 1) : h

check :: [(a, Int)] -> [Int]
check h = [findFreq 2, findFreq 3] where
  findFreq f = case findIndex (\(_, n) -> n == f) h of
    Just _  -> 1
    Nothing -> 0

main = do
  ids <- lines <$> readFile "input"
  let hs = fmap histo ids
      checked = fmap check hs
      [dubs, trips] = transpose checked
      checksum = sum dubs * sum trips
  putStrLn $ "Part 1: " ++ show checksum

