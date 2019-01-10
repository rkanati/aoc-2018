
module Main where

import Data.List (findIndex, transpose, elemIndex, intersect)

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

dist :: String -> String -> Int
dist a b = length $ filter id $ zipWith (/=) a b

main = do
  bids <- lines <$> readFile "input"

  let hs = fmap histo bids
      checked = fmap check hs
      [dubs, trips] = transpose checked
      checksum = sum dubs * sum trips
  putStrLn $ "Part 1: " ++ show checksum

  let matrix = zipWith (\i bid -> (i, fmap (dist bid) (drop (i+1) bids))) [0..] bids
      adjs = filter (\(i, row) -> 1 `elem` row) matrix
      (idxA, row) = head adjs
      Just offB = elemIndex 1 row
      bidA = bids !! idxA
      bidB = bids !! (idxA + offB + 1)
      common = intersect bidA bidB

  putStrLn $ "Part 2: Adjacent pairs: " ++ show (length adjs)
  putStrLn $ "        (A: " ++ bidA ++ ") (B: " ++ bidB ++ ")"
  putStrLn $ "        Common: " ++ common
  putStrLn $ "        |A| - |Common| = " ++ show (length bidA - length common)

