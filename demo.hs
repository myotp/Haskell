myDouble x = x + x

doubleSmallNumber x = if x > 10
                      then x
                      else x * 2

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, elem c ['A'..'Z']]

sayMe :: (Integral a) => a -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe x = "Not between 1 and 3"

myFac :: (Integral a) => a -> a
myFac 0 = 1
myFac n = n * myFac (n-1)

funType :: (Num a, Ord b) => a -> b -> b -> a
funType num a1 a2 = num + orderValue a1 a2
    where orderValue x y
              | x > y = 1
              | x == y = 0
              | x < y = -1

head' :: [a] -> a
head' [] = error "Can't call head on an empty list"
head' (x:_) = x

demoGuard :: (Integral a) => a -> a
demoGuard x
    | x < 0 = -x
    | x == 0 = 888
    | otherwise = x + 5

demoWhere :: (Integral a) => a -> String
demoWhere x
    | square < 10 = "0~3"
    | square == 16 = "4"
    | otherwise = "other number"
    where square = x * x

foo x y z = case compare x y of GT -> 1 + z
                                EQ -> 2 + z
                                LT -> 3 + z
