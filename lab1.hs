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

main :: IO()
main = do
  print (length (par_scanl1 1000 (+) [1..10000000]))
--  print (length (scanl1 (+) [1..10000000]))
