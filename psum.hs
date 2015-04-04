import Control.Parallel.Strategies
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import System.Random (StdGen, getStdGen, randoms)

psum xs = runEval $ do
  let (as, bs) = splitAt (length xs `div` 2) xs
  a <- rpar (sum as)
  b <- rpar (sum bs)
  rseq a
  rseq b
  return (a+b)

main :: IO ()
main = do
   print (psum [1..100000000])

-- <<main
main = do
  [n] <- getArgs
  let test = [test1,test2,test3,test4] !! (read n - 1)
  t0 <- getCurrentTime
  r <- evaluate (runEval test)
  printTimeSince t0
  print r
  printTimeSince t0
-- >>
