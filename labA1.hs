{-|
How to run:
ghc -O2 -threaded -rtsopts -eventlog labA1.hs
./labA1 0 +RTS -N1 -l -RTS     ## Linear scanl1
./labA1 1 +RTS -N2 -l -RTS     ## Simple 2 jobs in parallel

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
import Control.DeepSeq

-- linear implementation as a reference
lscanl :: (a -> b -> a) -> a -> [b] -> [a]
lscanl f q [] = [q]
lscanl f q (x:xs) = q:(lscanl f (f q x) xs)

lscanl1 :: (a -> a -> a) -> [a] -> [a]
lscanl1 f (x:xs) = lscanl f x xs

-- parallel implementation using Eval monad
-- simple divide the job into 2 parts and solve each part in parallel
-- See [Real World Haskell, Ch2, sudoku2.hs]
pscanl1 _ [] = error "pscanl1: empty list"
pscanl1 _ [x] = [x]
pscanl1 f xs = do
   runEval $ do
     as' <- rpar (force (lscanl1 f as))
     bs' <- rpar (force (lscanl1 f bs))
     rseq as'
     rseq bs'
     return (as' ++ (map (f (last as')) bs'))
   where (as, bs) = splitAt (length xs `div` 2) xs

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
par_scanl2 f xs = par_scanl2' 20 f xs
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
  | a < 0 = -1 * (slow_plus' (abs a) b 1)
  | otherwise = slow_plus' a b 1

slow_plus' a b x
  | a == 0 = b
  | a < 0 = a + b
  | otherwise = slow_plus' (a-x) (b+x) x

test_data = [1000000..1000200]
--test_data = [100..600]

-- default linear scanl1
scan0 :: [Int]
scan0 = force xs
  where xs = lscanl1 slow_plus test_data

-- parallel implementation simple 2 parts
scan1 :: [Int]
scan1 = force xs
  where xs = pscanl1 slow_plus test_data

-- parallel implementation with par/pseq
scan2 :: [Int]
scan2 = force xs
  where xs = par_scanl1 slow_plus test_data

-- parallel implementation with rpar
scan3 :: [Int]
scan3 = force xs
  where xs = par_scanl2 slow_plus test_data

-- mainly copied rpar.hs from PCPH
-- <<main
main = do
  [n] <- getArgs
  let scan_fun = [scan0, scan1, scan2, scan3] !! (read n)
  t0 <- getCurrentTime
  printTimeSince t0
  print (length scan_fun)
  printTimeSince t0

-- >>
printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)
