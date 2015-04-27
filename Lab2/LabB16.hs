import Data.List
import Data.Ord

type BuySellResult = (Int, Int, Int)

buySell :: [Int] -> BuySellResult
buySell xs = buySell' (0, 0, 0) xs 0

buySell' :: BuySellResult -> [Int] -> Int -> BuySellResult
buySell' result [] currentDay = result
buySell' (b, s, maxProfit) (x:xs) currentDay =
    if maxProfit' >= maxProfit
        then buySell' (b', s', maxProfit') xs (currentDay+1)
        else buySell' (b, s, maxProfit) xs (currentDay+1)
    where (b', s', maxProfit') = findMaxProfit currentDay x xs

findMaxProfit currentDay x [] = (0, 0, 0) -- last day
findMaxProfit currentDay x xs =
   (currentDay, currentDay + sellDay + 1, maxProfit)
    where (maxPrice, sellDay) = maxi xs 0
          maxProfit = maxPrice - x

maxi [] _ = (0, 0)
maxi (x:xs) i =
    if x >= fst xy'
        then (x, i)
        else xy'
    where xy' = maxi xs (i+1)
