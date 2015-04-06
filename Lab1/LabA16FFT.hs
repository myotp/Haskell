module LabA16FFT
(
    generate2DSamplesList
  , fft
  , par_monad_fft
) where

import Data.Complex
import Data.Array
import Data.Bits
import System.Random
import Data.Random.Normal
import Control.Parallel
import Control.Monad.Par

-- generating input for FFT or DFT. Borrowed from Simon Marlow I believe.
mX, mY, sdX, sdY :: Float
mX = 0
mY = 0
sdX = 0.5
sdY = 1.5

generate2DSamplesList :: Int           -- number of samples to generate
                  -> Float -> Float    -- X and Y mean
                  -> Float -> Float    -- X and Y standard deviations
                  -> IO [Complex Float]
generate2DSamplesList n mx my sdx sdy = do
  gen <- getStdGen
  let (genx, geny) = split gen
      xsamples = normals' (mx,sdx) genx
      ysamples = normals' (my,sdy) geny
  return $ zipWith (:+) (take n xsamples) ysamples

-- twiddle factors
tw :: Int -> Int -> Complex Float
tw n k = cis (-2 * pi * fromIntegral k / fromIntegral n)

fft :: [Complex Float] -> [Complex Float]
fft [a] = [a]
fft as = interleave ls rs
  where
    (cs,ds) = bflyS as
    ls = fft cs
    rs = fft ds

interleave [] bs = bs
interleave (a:as) bs = a : interleave bs as

bflyS :: [Complex Float] -> ([Complex Float], [Complex Float])
bflyS as = (los,rts)
  where
    (ls,rs) = halve as
    los = zipWith (+) ls rs
    ros = zipWith (-) ls rs
    rts = zipWith (*) ros [tw (length as) i | i <- [0..(length ros) - 1]]

-- missing from original file
halve as = splitAt n' as
  where
    n' = div (length as + 1) 2

------- ====== par/pseq Implementation ======= ------------
joinfft :: ([Complex Float], [Complex Float]) -> [Complex Float]
joinfft xys = interleave (fst xys) (snd xys)

divConq ::     -- is the problem big enough to be divided?
                ([Complex Float] -> Bool)            
               -- split
            -> ([Complex Float] -> ([Complex Float], [Complex Float]))  
               -- join
            -> (([Complex Float], [Complex Float]) -> [Complex Float])
               -- solve a sub-problem
            -> ([Complex Float] -> ([Complex Float],[Complex Float]))
            -> ([Complex Float] -> [Complex Float])

divConq granfilter split join f prob 
        | granfilter prob == True = fft prob
        | granfilter prob == False = par (fst ps) $ pseq (snd ps) $ 
                join ( (divConq granfilter split join f (fst ps))
                    , (divConq granfilter split join f (snd ps))  )
  where us = split prob
        vs0 = f (fst us)
        vs1 = f (snd us)
        ps = (join (fst vs0, snd vs0), join ( fst vs1, snd vs1))    
    
------- ====== Par Monad Implementation ======= ------------
par_monad_fft :: [Complex Float] -> [Complex Float]
par_monad_fft [a] = [a]
par_monad_fft as
  | length as < 200 = fft as     -- threshold to call original sequential code
  | otherwise = runPar $ do
  i <- new -- ls
  j <- new -- rs
  let (cs,ds) = par_monad_bflyS as
  fork (put i (par_monad_fft cs))
  fork (put j (par_monad_fft ds))
  ls <- get i
  rs <- get j
  return (interleave ls rs)

par_monad_bflyS :: [Complex Float] -> ([Complex Float], [Complex Float])
par_monad_bflyS as
  | length as < 100 = bflyS as  -- threshold to call original sequential code
  | otherwise = runPar $ do
  let  (ls,rs) = halve as
  i <- new -- los
  j <- new -- ros
  k <- new -- rts
  fork (put j (zipWith (-) ls rs))
  ros <- get j
  let ros' = tw' (length as) (length ros)
  fork (put k ( zipWith (*) ros ros'))
  fork (put i (zipWith (+) ls rs))
  los <- get i
  rts <- get k
  return (los,rts)

tw' la lr =  concat $ tw'' la lr (chunkdivide 1000 [0..(lr) - 1])

tw'' length_as length_ros chunks =
    runPar (parMap (map (tw (length_as))) chunks)

chunkdivide :: Int -> [a] -> [[a]]
chunkdivide _ [] = []
chunkdivide n xs = take n xs : chunkdivide n (drop n xs)
