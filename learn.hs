divide xs = do
  return (a:as ++ b:bs)
  where ((a:as), (b:bs)) = splitAt (length xs `div` 2) xs
