main :: IO ()
main = do
   print (length (scanl1 (+) [1..100000000]))
