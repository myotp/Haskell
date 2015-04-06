{-|
This file will simplify the job to generate eventlog for ThreadScope

How to run:
ghc -O2 -threaded -rtsopts -eventlog LabA16FFTMain.hs
./LabA16FFTMain 0 200000 +RTS -N1 -l -A1m -RTS  ## Sequential
./LabA16FFTMain 1 200000 +RTS -N2 -l -A1m -RTS  ## Par Monad on 2cores
./LabA16FFTMain 1 200000 +RTS -N4 -l -A1m -RTS  ## Par Monad on 4cores
./LabA16FFTMain 0 200000 +RTS -N1 -l -A1000m -RTS  ## Sequential
./LabA16FFTMain 1 200000 +RTS -N2 -l -A1000m -RTS  ## Par Monad on 2cores
./LabA16FFTMain 1 200000 +RTS -N4 -l -A1000m -RTS  ## Par Monad on 4cores

-}

import Data.Time.Clock
import Text.Printf
import System.Environment
import LabA16FFT

run_sequential_fft n = do
  xs <- generate2DSamplesList n 0 0 0.5 1.5
  return (fft xs)

run_par_monad_fft n = do
  xs <- generate2DSamplesList n 0 0 0.5 1.5
  return (par_monad_fft xs)

main = do
  [n, m] <- getArgs
  let fft_fun = [run_sequential_fft, run_par_monad_fft] !! (read n)
  t0 <- getCurrentTime
  printTimeSince t0
  xs <- fft_fun (read m)
  print (sum xs)
  printTimeSince t0

-- >>
printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)
