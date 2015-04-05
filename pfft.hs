import Data.Complex
import Data.Array
import Data.Bits
import System.Random
import Data.Random.Normal
import Criterion.Main
import Control.Parallel
import Control.Monad.Par
import Data.Time.Clock
import Text.Printf
import System.Environment


-- file given.hs for use with Lab 1 Part 1 of the Chalmers PFP Course
-- Please write your names in the file if submitting it

-- generating input for FFT or DFT. Borrowed from Simon Marlow I believe.
mX, mY, sdX, sdY :: Float
mX = 0
mY = 0
sdX = 0.5
sdY = 1.5

generate2DSamplesList :: Int           -- number of samples to generate
                  -> Float -> Float    -- X and Y mean
                  -> Float -> Float    -- X and Y standard deviations
                  -> IO [Complex Float]
generate2DSamplesList n mx my sdx sdy = do
  gen <- getStdGen
  let (genx, geny) = split gen
      xsamples = normals' (mx,sdx) genx
      ysamples = normals' (my,sdy) geny
  return $ zipWith (:+) (take n xsamples) ysamples

-- Task 1
divConq :: (prob -> Bool)              -- is the problem indivisible?
            -> (prob -> [prob])        -- split
            -> ([sol] -> sol)          -- join
            -> (prob -> sol)           -- solve a sub-problem
            -> (prob -> sol)

divConq indiv split join f prob = undefined



-- Task 2


-- twiddle factors
tw :: Int -> Int -> Complex Float
tw n k = cis (-2 * pi * fromIntegral k / fromIntegral n)

dft :: [Complex Float] -> [Complex Float]
dft xs = [ sum [ xs!!j * tw n (j*k) | j <- [0..n']] | k <- [0..n']]
  where
    n = length xs
    n' = n-1



-- In case you are wondering, this is the Decimation in Frequency (DIF) 
-- radix 2 Cooley-Tukey FFT

fft :: [Complex Float] -> [Complex Float]
fft [a] = [a]
fft as = interleave ls rs
  where
    (cs,ds) = bflyS as
    ls = fft cs
    rs = fft ds

interleave [] bs = bs
interleave (a:as) bs = a : interleave bs as

bflyS :: [Complex Float] -> ([Complex Float], [Complex Float])
bflyS as = runPar $ do
  let  (ls,rs) = halve as
  i <- new -- los
  j <- new -- ros
  fork (put i (zipWith (+) ls rs))
  fork (put j (zipWith (-) ls rs))
  los <- get i
  ros <- get j
  let  rts = zipWith (*) ros [tw (length as) i | i <- [0..(length ros) - 1]]
  return (los,rts)

-- missing from original file
halve as = splitAt n' as
  where
    n' = div (length as + 1) 2

test0 = do
  xs <- generate2DSamplesList 50000 0 0 0.5 1.5
  return (fft xs)

run_default_fft n = do
  xs <- generate2DSamplesList n 0 0 0.5 1.5
  return (fft xs)

main0 = do
  xs <- run_default_fft 50000
  print (sum xs)

-- mainly copied rpar.hs from PCPH
-- <<main
main = do
  [n, m] <- getArgs
  let fft_fun = [run_default_fft] !! (read n)
  t0 <- getCurrentTime
  printTimeSince t0
  xs <- fft_fun (read m)
  print (sum xs)
  printTimeSince t0

printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)

{-|
ghc -O2 -threaded -rtsopts -eventlog pfft.hs
./pfft 0 200 +RTS -N1 -l -RTS     ## default fft
./pfft 0 200000 +RTS -N2 -l -A100m -RTS     ## default fft
-}
