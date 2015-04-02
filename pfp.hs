map0 :: (a -> b) -> [a] -> [b]
map0 f [] = []
map0 f (x:xs) = f x : map0 f xs
