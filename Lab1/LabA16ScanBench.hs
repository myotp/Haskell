{-|
ghc -O2 -threaded LabA16ScanBench.hs
./LabA16ScanBench +RTS -N2 -RTS --output LabA16ScanBenchResult.html
-}
import Criterion.Main
import LabA16Scan

-- seems Criterion can't generate html report for long jobs
-- so, we shrink the input here to use Criterion to generate report.
test_data :: [Int]
test_data = [1000000..1000050]

main = defaultMain [
         bench "Seq scanl1" (nf (\x -> seq_scanl1 slow_plus x) test_data)
       , bench "Par Monad" (nf (\x -> par_monad_scan slow_plus x) test_data)
       , bench "Eval" (nf (\x -> par_eval_scan slow_plus x) test_data)
       ]
