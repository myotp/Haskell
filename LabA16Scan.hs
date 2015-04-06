module LabA16Scan
(
  lscanl1, slow_plus, par_monad_scan
) where

import Control.Parallel
import Control.Exception
import Data.Time.Clock
import Text.Printf
import System.Environment
import Control.Parallel.Strategies -- hiding (parMap)

import Control.Parallel (par, pseq)
import Control.DeepSeq


import Control.Monad.Par
import Stream

-- sequential implementation as a reference
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

-- use strategy
par_scanl_strategy f xs = par_scanl_strategy' 10 f xs
par_scanl_strategy' chunksize f xs =
   concat (combine f xs')
   where chunks = chunkdivide chunksize xs
         xs' = Control.Parallel.Strategies.parMap rpar (scanl1 f) chunks

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
slow_plus :: Int -> Int -> Int
slow_plus a b
  | a < 0 = -1 * (slow_plus' (abs a) b 1)
  | otherwise = slow_plus' a b 1

slow_plus' a b x
  | a == 0 = b
  | a < 0 = a + b
  | otherwise = slow_plus' (a-x) (b+x) x

par_monad_scan f xs =
  par_monad_scan' f (chunkdivide 4 xs)

par_monad_scan' f chunks = runPar $ do
  s0 <- streamFromList chunks
  s1 <- streamMap (scanl1 f) s0
  xs <- streamFold (\x y -> fold_combine f x y) [] s1
  return xs

--fold_combine :: (a -> a -> a) -> [a] -> [a] -> [a]
fold_combine f [] bs = bs
fold_combine f as bs = -- as ++ map (f (last as)) bs
  as ++ runPar (Control.Monad.Par.parMap (f (last as)) bs)
