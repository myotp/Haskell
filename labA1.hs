{-|
How to run:
ghc -O2 -threaded -rtsopts -eventlog labA1.hs
./labA1 1 +RTS -N1 -l -RTS     ## Linear scanl1
./labA1 2 +RTS -N2 -l -RTS     ## parallel scanl1
./labA1 2 +RTS -N2 -l -RTS     ## parallel scanl1
./labA1 3 +RTS -N2 -l -RTS     ## parallel scanl1
-}

import Control.Parallel
import Control.Exception
import Data.Time.Clock
import Text.Printf
import System.Environment
import Control.Parallel.Strategies

import Control.Parallel (par, pseq)

{-| parallel implmentation
    simply recursive solution
-}
par_scanl1 _ [] = error "pscanl1: empty list"
par_scanl1 _ [x] = [x]
par_scanl1 f xs = do
   force' as' `par` (force' bs' `pseq` (as' ++ (map (f (last as')) bs')))
   where (as, bs) = splitAt (length xs `div` 2) xs
         as' = par_scanl1 f as
         bs' = par_scanl1 f bs

force' :: [a] -> ()
force' xs = go xs `pseq` ()
    where go (_:xs) = go xs
          go [] = 1

{-| parallel implmentation
    divide the job into small chunks and run each chunk in parallel
-}
par_scanl2 f xs = par_scanl2' 4 f xs
par_scanl2' chunksize f xs =
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
-- Since (+) is a so cheap operation, our parallel program can't beat it
-- because it will spend much more time to create small jobs in memory.
slow_plus a b
  | a < 0 = -1 * (slow_plus' (abs a) b)
  | otherwise = slow_plus' a b

slow_plus' a b
  | a == 0 = b
  | a < 0 = a + b
  | otherwise = slow_plus' (a-1) (b+1)

test_data = [1000..2000]

-- default linear scanl1
scan1 = sum (scanl1 slow_plus test_data)

-- parallel implementation with par/pseq
scan2 = sum (par_scanl1 slow_plus test_data)

-- parallel implementation with rpar
scan3 = sum (par_scanl2 slow_plus test_data)

-- mainly copied rpar.hs from PCPH
-- <<main
main = do
  [n] <- getArgs
  let scan_fun = [scan1, scan2, scan3] !! (read n - 1)
  t0 <- getCurrentTime
  printTimeSince t0
  print scan_fun
  printTimeSince t0
-- >>
printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)
