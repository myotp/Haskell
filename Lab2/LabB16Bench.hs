{-|
ghc -O2 -threaded LabB16Bench.hs
./LabB16Bench +RTS -N2 -RTS --output LabB16BenchResult.html
-}
import Criterion.Main
import LabB16

test_data :: [Int]
test_data = [1..10000]

main = defaultMain [
         bench "Seq" (nf seqBuySell test_data)
       , bench "Parallel" (nf parBuySell test_data)
       ]
