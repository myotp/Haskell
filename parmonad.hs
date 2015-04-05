import Control.Exception
import System.Environment
import Control.Monad.Par.Scheds.Trace
-- NB. using Trace here, Direct is too strict and forces the fibs in
-- the parent; see https://github.com/simonmar/monad-par/issues/27

-- <<fib
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
-- >>

main = do
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
      a <- get i
      b <- get j
      return (a+b)

par_scan_simple f [a,b] =
    runPar $ do
      i <- new
      j <- new
      fork (put i (scanl1 f a))
      fork (put j (scanl1 f b))
      a' <- get i
      b' <- get j
      return (combine f a' b')

combine :: (a -> a -> a) -> [a] -> [a] -> [a]
combine f as bs = as ++ bs'
   where bs' = map (f (last as)) bs

demo_list :: [Int] -> Int
demo_list [a, b] = a + b
demo_list [a, b, c] = a + b + c

{-|
:l parmonad
par_scan_simple (+) [ [1,2], [3,4] ]
-}

