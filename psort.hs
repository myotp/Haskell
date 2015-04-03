-- REF1: rpar.hs from PCPH
-- REF2: SortMain.hs from Real World Haskell

import Control.Parallel.Strategies
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import System.Random (StdGen, getStdGen, randoms)
import Control.Parallel (par, pseq)
import Text.Printf

-- Original linear sort from Real World Haskell, Chapter 24
sort0 :: (Ord a) => [a] -> [a]
sort0 (x:xs) = lesser ++ x:greater
    where lesser  = sort0 [y | y <- xs, y <  x]
          greater = sort0 [y | y <- xs, y >= x]
sort0 _ = []

-- helper functions
randomInts :: Int -> StdGen -> [Int]
randomInts k g = let result = take k (randoms g)
                 in force result `seq` result

force :: [a] -> ()
force xs = go xs `pseq` ()
    where go (_:xs) = go xs
          go [] = 1

-- <<main
main = do
  [n] <- getArgs
  let sort_fun = [sort0] !! (read n - 1)
  input <- randomInts 500000 `fmap` getStdGen
  t0 <- getCurrentTime
  let sorted = sort_fun input
  printTimeSince t0
  print (length (sorted))
  printTimeSince t0
-- >>

printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)
