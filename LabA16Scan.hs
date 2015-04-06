module LabA16Scan
(
    lscanl1
  , slow_plus
  , par_monad_scan
) where

import Control.Monad.Par
import Stream

-- sequential implementation as a reference
lscanl1 :: (a -> a -> a) -> [a] -> [a]
lscanl1 f (x:xs) = lscanl f x xs

lscanl :: (a -> b -> a) -> a -> [b] -> [a]
lscanl f q [] = [q]
lscanl f q (x:xs) = q:(lscanl f (f q x) xs)

------ ===== Par Monad Implementation =======
par_monad_scan :: NFData a => (a -> a -> a) -> [a] -> [a]
par_monad_scan f xs =
  par_monad_scan' f (chunkdivide 4 xs)

par_monad_scan' f chunks = runPar $ do
  s0 <- streamFromList chunks
  s1 <- streamMap (scanl1 f) s0
  xs <- streamFold (\x y -> fold_combine f x y) [] s1
  return xs

fold_combine :: NFData a => (a -> a -> a) -> [a] -> [a] -> [a]
fold_combine f [] bs = bs
fold_combine f as bs =
  as ++ runPar (Control.Monad.Par.parMap (f (last as)) bs)

chunkdivide :: Int -> [a] -> [[a]]
chunkdivide _ [] = []
chunkdivide n xs = take n xs : chunkdivide n (drop n xs)

{-|
Magic expensive associative operator
Since (+) is a so cheap operation, our parallel program can't beat it
because it will spend much more time to create small jobs in memory.
-}
slow_plus :: Int -> Int -> Int
slow_plus a b
  | a < 0 = -1 * (slow_plus' (abs a) b 1)
  | otherwise = slow_plus' a b 1

slow_plus' a b x
  | a == 0 = b
  | a < 0 = a + b
  | otherwise = slow_plus' (a-x) (b+x) x
