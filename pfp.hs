map0 :: (a -> b) -> [a] -> [b]
map0 f [] = []
map0 f (x:xs) = f x : map0 f xs

-- explicit thunk
map1 :: (a -> b) -> [a] -> [b]
map1 f [] = []
map1 f (x:xs) = let
                     x'  = f x
                     xs' = map f xs
                in
                     x' : xs'
