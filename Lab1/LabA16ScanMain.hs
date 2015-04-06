{-|
This file will simplify the job to generate eventlog for ThreadScope

How to run:
ghc -O2 -threaded -rtsopts -eventlog LabA16ScanMain.hs
./LabA16ScanMain 0 +RTS -N1 -l -RTS     ## Sequential scanl1
./LabA16ScanMain 1 +RTS -N2 -l -RTS     ## Par Monad
-}

import Data.Time.Clock
import Text.Printf
import System.Environment
import LabA16Scan

test_data :: [Int]
test_data = [1000000..1000200]

-- default sequential scanl1
scan0 :: [Int]
scan0 = lscanl1 slow_plus test_data

-- parallel implementation with Par Monad
scan1 :: [Int]
scan1 = par_monad_scan slow_plus test_data

-- mainly copied rpar.hs from PCPH
-- <<main
main = do
  [n] <- getArgs
  let scan_fun = [scan0, scan1] !! (read n)
  t0 <- getCurrentTime
  printTimeSince t0
  print (sum (scan_fun))
  printTimeSince t0

-- >>
printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)
