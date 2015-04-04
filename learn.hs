import Control.Parallel.Strategies

divide xs = do
  return (a:as ++ b:bs)
  where ((a:as), (b:bs)) = splitAt (length xs `div` 2) xs

-- Eval Monad, rpar and rseq
divide2 xs = runEval (divide2' xs)

divide2' xs = do
  as' <- rpar (map (+1) (a:as))
  rseq as'
  return (as' ++ b:bs)
  where ((a:as), (b:bs)) = splitAt (length xs `div` 2) xs

magic_sum xs
  | length xs < 4 = 88
  | otherwise = sum xs
