import Control.Parallel

par_scanl1 _ [] = error "pscanl1: empty list"
par_scanl1 f xs = do
  let lastElement = last as
  as ++ (map (f lastElement) bs)
  where (as,bs) = mypscan f xs

mypscan :: (a -> a -> a) -> [a] -> ([a], [a])
mypscan f xs = do
  force first_half `par` (force second_half `pseq` (first_half, second_half))
  where ((a:as), (b:bs)) = splitAt (length xs `div` 2) xs
        first_half = scanl f a as
        second_half = scanl f b bs

main :: IO ()
main = do
  print (length ((par_scanl1 (+) [1..10000000])::[Int]))

force :: [a] -> ()
force xs = go xs `pseq` ()
    where go (_:xs) = go xs
          go [] = 1
