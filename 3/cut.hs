
module Main where

import Data.List (groupBy, find)
import Data.Char (isDigit)
import qualified Data.Vector.Unboxed         as V
import           Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Traversable (for)

data Rect = Rect
  { x0, y0, x1, y1 :: !Int }
  deriving Show

data Claim = Claim
  { cid :: !Int, crect :: !Rect }
  deriving Show

parse :: String -> Claim
parse s = Claim cid (Rect rx0 ry0 rx1 ry1) where
  ["#", rawCid, _, rawX0, _, rawY0, _, rawW, _, rawH] = groupBy pred s
  pred c d = isDigit c == isDigit d
  cid = read rawCid
  rx0 = read rawX0
  ry0 = read rawY0
  rx1 = rx0 + read rawW
  ry1 = ry0 + read rawH

main = do
  claims <- (fmap parse . lines) <$> readFile "input"
  let gridW = maximum $ fmap (x1 . crect) claims
      gridH = maximum $ fmap (y1 . crect) claims
  putStrLn $ "Bounds: " ++ show gridW ++ ", " ++ show gridH

  let gridIndices (Rect rx0 ry0 rx1 ry1)
        = [ y*gridW + x | x <- [rx0 .. rx1-1], y <- [ry0 .. ry1-1] ]

      rects = fmap crect claims
      grid :: V.Vector Int
      grid = V.create $ do
        -- grid full of zeros
        grid <- VM.replicate (gridW * gridH) (0 :: Int)

        -- for each claim, add one to each cell covered by the claim
        for rects $ \r -> for (gridIndices r) (VM.modify grid (+ 1))

        pure grid

  -- count cells >= 2
  let nLapped = V.foldr' (\c -> if c >= 2 then (+ 1) else id) 0 grid
  putStrLn $ "Part 1: " ++ show nLapped ++ " sq in"

  -- find nonlapping claim
  let nonLapping c = all (== 1) cells where
        cells = fmap (grid !) (gridIndices (crect c))
      Just (Claim resultCid _) = find nonLapping claims
  putStrLn $ "Part 2: #" ++ show resultCid

