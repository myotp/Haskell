type BuySellResult = (Int, Int, Int)

-- sequential solution
findMaxProfit :: [Int] -> BuySellResult
findMaxProfit priceList = findMaxProfit' (0, 0, 0) dayIndexes priceList
   where dayIndexes = [0..((length priceList) - 1)]

findMaxProfit' buySellResult [] _ = buySellResult
findMaxProfit' (b, s, p) (b':bs) ps
   | p' >= p = findMaxProfit' (b', s', p') bs ps
   | otherwise = findMaxProfit' (b, s, p) bs ps
   where (s', p') = findMaxProfitForDay ps b'

-- find a maximum profit with a specific buy day
-- returns (sell_day, profit)
findMaxProfitForDay :: [Int] -> Int -> (Int, Int)
findMaxProfitForDay priceList buyDay
   | (length priceList) - 1 == buyDay = (0, 0) -- last day
   | otherwise = (sellDay, maxPrice - (priceList !! buyDay))
   where (maxPrice, sellDay) = maxi priceList buyDay

maxi :: [Int] -> Int -> (Int, Int)
maxi (x:xs) startIndex = maxi' (x, 0) startIndex $ zip xs [1..]

maxi' (x, i) _ [] = (x, i)
maxi' (x, i) i0 ((x',i'):xs)
    | (x' > x) && (i' > i0) = maxi' (x', i') i0 xs
    | otherwise = maxi' (x, i) i0 xs
