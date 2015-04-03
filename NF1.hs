-- Lecture 1, Using par
import Control.Parallel

nfib :: Integer -> Integer
nfib n | n < 2 = 1
nfib n = par nf (nf + nfib (n-2) + 1)
         where nf = nfib (n-1)

main = print (nfib 40)
