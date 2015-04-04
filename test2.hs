import Control.Parallel
import Control.Exception
import Data.Time.Clock
import Text.Printf
import System.Environment
import Control.Parallel.Strategies

import Control.Parallel (par, pseq)
import Control.DeepSeq

pscan :: (a -> a -> a) -> [[a]] -> Eval [[a]]
pscan f chunks = pmap (scanl1 f) chunks

pmap :: (a -> b) -> [a] -> Eval [b]
pmap f [] = return []
pmap f (a:as) = do
  b <- rpar (f a)
  bs <- pmap f as
  return (b:bs)

