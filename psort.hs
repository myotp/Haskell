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

-- My naive 'parallel' sort, only parallel in first step
-- This is where I got total Spart 1 and converted 0
sort1 :: (Ord a) => [a] -> [a]
sort1 (x:xs) = force greater `par` (force lesser `pseq`
                                    (lesser ++ x:greater))
    where lesser  = sort0 [y | y <- xs, y <  x] -- call linear sort
          greater = sort0 [y | y <- xs, y >= x] -- call linear sort
sort1 _ = []

-- use par recursively to generate many smaller parallel jobs
-- parSort from Real World Haskell, Chapter 24
sort2 :: (Ord a) => [a] -> [a]
sort2 (x:xs) = force greater `par` (force lesser `pseq`
                                    (lesser ++ x:greater))
    where lesser  = sort2 [y | y <- xs, y <  x] -- par recursively
          greater = sort2 [y | y <- xs, y >= x] -- par recursively
sort2 _ = []

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
  let sort_fun = [sort0, sort1, sort2] !! (read n - 1)
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

{-|
ghc -O2 -threaded -rtsopts -eventlog psort.hs
./psort 2 +RTS -N2 -l -RTS
-}
