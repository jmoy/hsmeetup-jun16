{-# LANGUAGE BangPatterns #-}
module Main (main) where

import System.Environment (getArgs)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((//),(!))

main::IO ()
main = do
  [a1] <- getArgs
  let n = read a1
  putStrLn $ "Count = "++show (countPrimes n)

countPrimes::Int->Int
countPrimes n = V.length $ V.filter id $ loop 2 mask0
  where
    mask0 = (V.replicate n True) // [(0,False),(1,False)]
    loop p mask 
      | p == n = mask
      | not (mask ! p) = loop (p+1) mask
      | otherwise = loop (p+1) mask'
        where
          mask' = mask // [(k,False)|k<-[p*p,p*p+p..(n-1)]]

