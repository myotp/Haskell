import Control.Parallel.Strategies
import Control.DeepSeq

pscanl :: (a -> b -> a) -> a -> [b] -> [a]
pscanl f q [] = [q]
pscanl f q (x:xs) = q:pscanl f (f q x) xs

pscanl1 :: (a -> a -> a) -> [a] -> [a]
pscanl1 _ [] = error "pscanl1: empty list"
pscanl1 f xs = runEval (mypscan f xs)


mypscan :: (a -> a -> a) -> [a] -> Eval [a]
mypscan f xs = do
  as' <- rpar (scanl f a as)
  bs' <- rpar (scanl f b bs)
  rseq as'
  rseq bs'
  return (as' ++ bs')
  where ((a:as), (b:bs)) = splitAt (length xs `div` 2) xs


foo :: a -> [a] -> [a]
foo a [] = []
