import Data.Time.Clock
import Text.Printf
import Control.Exception
import System.Environment
import Control.Monad.Par.Scheds.Trace

import Control.Monad.Par

-- NB. using Trace here, Direct is too strict and forces the fibs in
-- the parent; see https://github.com/simonmar/monad-par/issues/27

-- <<fib
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
-- >>

main0 = do
  args <- getArgs
  let [n,m] = map read args
  print $
-- <<runPar
    runPar $ do
      i <- new                          -- <1>
      j <- new                          -- <1>
      fork (put i (fib n))              -- <2>
      fork (put j (fib m))              -- <2>
      a <- get i                        -- <3>
      b <- get j                        -- <3>
      return (a+b)                      -- <4>
-- >>

demo_par x y =
    runPar $ do
      i <- new
      j <- new
      fork (put i (x + y))
      fork (put j (x - y))
      a1' <- get i
      b <- get j
      return (a1'+b)

par_scan_simple f [a,b] =
    runPar $ do
      i1 <- new
      j <- new
      fork (put i1 (scanl1 f a))
      fork (put j (scanl1 f b))
      a' <- get i1
      b' <- get j
      let b'' = combine f a' b'
      return ([a', b''])

pscan f [x1,x2,x3,x4] =
  runPar $ do
    i1 <- new
    i2 <- new
    i3 <- new
    i4 <- new

    fork (put i1 (scanl1 f x1))
    fork (put i2 (scanl1 f x2))
    fork (put i3 (scanl1 f x3))
    fork (put i4 (scanl1 f x4))

    x1' <- get i1
    x2' <- get i2
    let x2'' = combine f x1' x2'
    x3' <- get i3
    let x3'' = combine f x2'' x3'
    x4' <- get i4
    let x4'' = combine f x3'' x4'
    return [x1', x2'', x3'', x4'']

-- parallel scan with Par Monad, but linear combine
-- First try
pscan_lcombine f [x1,x2,x3,x4] =
  runPar $ do
    i1 <- new
    i2 <- new
    i3 <- new
    i4 <- new

    i2' <- new
    i3' <- new
    i4' <- new

    -- consumer
    fork (do x1' <- get i1; x2' <- get i2; put i2' (combine f x1' x2'))
    fork (do x2' <- get i2'; x3' <- get i3; put i3' (combine f x2' x3'))
    fork (do x3' <- get i3'; x4' <- get i4; put i4' (combine f x3' x4'))

    -- producer
    fork (do put i1 (scanl1 f x1); put i2 (scanl1 f x2); put i3 (scanl1 f x3); put i4 (scanl1 f x4))

    x1'' <- get i1
    x2'' <- get i2'
    x3'' <- get i3'
    x4'' <- get i4'
    return [x1'', x2'', x3'', x4'']

combine :: (a -> a -> a) -> [a] -> [a] -> [a]
combine f as bs = map (f (last as)) bs

par_scan_par_combine f [x1,x2,x3,x4] =
  runPar $ do
    i1 <- new
    i2 <- new
    i3 <- new
    i4 <- new

    i2' <- new
    i3' <- new
    i4' <- new

    -- consumer
    fork (do x1' <- get i1; x2' <- get i2; put i2' (par_combine f x1' x2'))
    fork (do x2' <- get i2'; x3' <- get i3; put i3' (par_combine f x2' x3'))
    fork (do x3' <- get i3'; x4' <- get i4; put i4' (par_combine f x3' x4'))

    -- producer
    fork (do put i1 (scanl1 f x1); put i2 (scanl1 f x2); put i3 (scanl1 f x3); put i4 (scanl1 f x4))

    x1'' <- get i1
    x2'' <- get i2'
    x3'' <- get i3'
    x4'' <- get i4'
    return [x1'', x2'', x3'', x4'']

par_combine :: NFData a => (a -> a -> a) -> [a] -> [a] -> [a]
par_combine f as bs =
    runPar (parMap (f (last as)) bs)


demo_list :: [Int] -> Int
demo_list [a, b] = a + b
demo_list [a, b, c] = a + b + c

{-|
:l parmonad
par_scan_simple (+) [ [1,2], [3,4] ]
concat (pscan (+) [ [1,2], [3,4], [5,6], [7,8] ])
scanl1 (+) [1..8]
chunkdivide 50 test_data
-}

test_data = [1000000..1000200]::[Int]
--test_data = [1..8]::[Int]

chunkdivide :: Int -> [a] -> [[a]]
chunkdivide _ [] = []
chunkdivide n xs = take n xs : chunkdivide n (drop n xs)

slow_plus a b
  | a < 0 = -1 * (slow_plus' (abs a) b 1)
  | otherwise = slow_plus' a b 1

slow_plus' a b x
  | a == 0 = b
  | a < 0 = a + b
  | otherwise = slow_plus' (a-x) (b+x) x

main = do
  [n] <- getArgs
  let scan_fun = [pscan_lcombine, par_scan_par_combine] !! ((read n) - 1)
  t0 <- getCurrentTime
  let chunk_size = (length test_data `div` 4) + 1
  print chunk_size
  let chunks = chunkdivide chunk_size test_data
  print (length chunks)
  printTimeSince t0
  let result = concat (scan_fun slow_plus chunks)
  print (sum (result))
  printTimeSince t0


printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)

{-|
ghc -O2 -threaded -rtsopts -eventlog parmonad.hs
./parmonad 1 +RTS -N2 -l -RTS   ## pipeline first try, Par scan, linear combine
./parmonad 2 +RTS -N2 -l -RTS   ## Par scan, Par combine
./parmonad 2 +RTS -N4 -l -RTS   ## Par scan, Par combine, N4
-}
