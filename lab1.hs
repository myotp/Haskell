{-|
  How to run:
  ghc -O2 -threaded -rtsopts -eventlog laba1.hs
  
-}

import Control.Parallel
import Control.Exception
import Data.Time.Clock
import Text.Printf
import System.Environment

import Control.Parallel.Strategies

par_scanl1 chunksize f xs =
   concat (combine f (runEval (pmap (scanl1 f) (chunkdivide chunksize xs))))

pmap :: (a -> b) -> [a] -> Eval [b]
pmap f [] = return []
pmap f (a:as) = do
  b <- rpar (f a)
  bs <- pmap f as
  return (b:bs)

combine :: (a-> a-> a) -> [[a]] -> [[a]]
combine f (x:[]) = [x]
combine f (x:y:xs) =
   x : combine f (map (f (last x)) y : xs)

chunkdivide :: Int -> [a] -> [[a]]
chunkdivide _ [] = []
chunkdivide n xs = take n xs : chunkdivide n (drop n xs)

-- Magic expensive associative operator
slow_plus a b
  | a < 0 = -1 * (slow_plus' (abs a) b)
  | otherwise = slow_plus' a b

slow_plus' a b
  | a == 0 = b
  | otherwise = slow_plus' (a-1) (b+1)

-- default linear scanl1
scan0 =
    sum (scanl1 slow_plus [100..150])

-- mainly copied rpar.hs from PCPH
-- <<main
main = do
  [n] <- getArgs
  let scan_fun = [scan0] !! (read n - 1)
  t0 <- getCurrentTime
  printTimeSince t0
  print scan_fun
  printTimeSince t0
-- >>
printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)

--main :: IO()
--main = do
--  print (length (par_scanl1 1000 (+) [1..10000000]))
--  print (length (scanl1 slow_plus [10000..10050]))

