
module Main where

import Data.List (groupBy)
import Data.Char (isDigit)
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Traversable (for)

data Rect = Rect
  { x0, y0, x1, y1 :: !Int }
  deriving Show

width  r = x1 r - x0 r
height r = y1 r - y0 r
area r = width r * height r

data Claim = Claim
  { cid :: !Int, crect :: !Rect }
  deriving Show

intersection :: Rect -> Rect -> Maybe Rect
intersection (Rect rx0 ry0 rx1 ry1) (Rect sx0 sy0 sx1 sy1)
  = if rx0 < sx1 && rx1 > sx0 && ry0 < sy1 && ry1 > sy0
      then Just rect
      else Nothing
  where
    rect = Rect (max rx0 sx0) (max ry0 sy0) (min rx1 sx1) (min ry1 sy1)

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

  let rects = fmap crect claims
      grid :: V.Vector Int
      grid = V.create $ do
        -- grid full of zeros
        grid <- VM.replicate (gridW * gridH) (0 :: Int)

        -- for each claim, add one to each cell covered by the claim
        for rects $ \(Rect rx0 ry0 rx1 ry1) -> do
          let pairs = (,) <$> [rx0 .. rx1 - 1] <*> [ry0 .. ry1 - 1]
              idxs = fmap (\(x, y) -> y * gridW + x) pairs
          for idxs $ VM.modify grid (+ 1)
        pure grid

  -- count cells >= 2
  let nLapped = V.foldl (\n c -> if c >= 2 then n + 1 else n) 0 grid
  putStrLn $ "Part 1: " ++ show nLapped

