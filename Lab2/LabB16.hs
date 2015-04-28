import Data.Array.Repa as Repa

type BuySellResult = (Int, Int, Int)

-- sequential solution
seqBuySell :: [Int] -> BuySellResult
seqBuySell priceList = seqBuySell' (0, 0, 0) dayIndexes priceList
   where dayIndexes = [0..((length priceList) - 1)]

seqBuySell' buySellResult [] _ = buySellResult
seqBuySell' (b, s, p) (b2:bs) ps
   | p' >= p = seqBuySell' (b', s', p') bs ps
   | otherwise = seqBuySell' (b, s, p) bs ps
   where (b', s', p') = findMaxProfitForDay ps b2

-- find a maximum profit with a specific buy day
-- returns (buy_day, sell_day, profit)
findMaxProfitForDay :: [Int] -> Int -> BuySellResult
findMaxProfitForDay priceList buyDay
   | (length priceList) - 1 == buyDay = (0, 0, 0) -- last day
   | otherwise = (buyDay, sellDay, maxPrice - (priceList !! buyDay))
   where (maxPrice, sellDay) = maxi priceList buyDay

maxi :: [Int] -> Int -> (Int, Int)
maxi (x:xs) startIndex = maxi' (x, 0) startIndex $ zip xs [1..]

maxi' (x, i) _ [] = (x, i)
maxi' (x, i) i0 ((x',i'):xs)
    | (x' > x) && (i' > i0) = maxi' (x', i') i0 xs
    | otherwise = maxi' (x, i) i0 xs

parPlus1 :: [Int] -> [Int]
parPlus1 xs =
    toList (computeS (Repa.map (+1) a) :: Array U DIM1 Int)
    where a = fromListUnboxed (Z :. (length xs)) xs :: Array U DIM1 Int

-- parallel solution
parBuySell xs =
   merge xs'
   where xs' = parBuySell' xs

merge (x:xs) = merge' x xs
merge' result [] = result
merge' (b,s,p) ((b',s',p'):xs)
   | p' >= p = merge' (b',s',p') xs
   | otherwise = merge' (b,s,p) xs

-- parallel
parBuySell' xs =
    toList (computeS (Repa.map (mapf) a) :: Array U DIM1 BuySellResult)
    where a = fromListUnboxed (Z :. (length buyDays)) buyDays :: Array U DIM1 Int
          buyDays = [0..(length xs - 1)]
          mapf = findMaxProfitForDay xs
