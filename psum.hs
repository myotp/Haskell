import Control.Parallel.Strategies

psum xs = runEval $ do
  a <- rpar (sum as)
  b <- rpar (sum bs)
  rseq a
  rseq b
  return (a+b)
  where (as, bs) = splitAt (length xs `div` 2) xs

main :: IO ()
main = do
   print (psum [1..10000000])
