{-# LANGUAGE BangPatterns #-}
module Main (main) where

import System.Environment (getArgs)
import qualified Data.Vector.Unboxed.Mutable as V
import Control.Monad.ST
import Control.Monad

main::IO ()
main = do
  [a1] <- getArgs
  let n = read a1
  putStrLn $ "Count = "++show (countPrimes n)

countPrimes::Int->Int
countPrimes n = runST $ do
  v <- V.replicate n True
  V.write v 0 False
  V.write v 1 False
  loop v 2
  count v 0 0
  where
    loop !v p 
      | p==n = return ()
      | otherwise = do
          b <- V.read v p
          when b $ markMultiples v p (p*p)
          loop v (p+1)
    markMultiples !v p i 
      | i >= n = return ()
      | otherwise = do
          V.write v i False
          markMultiples v p (i+p)
    count !v !c i
      | i==n = return c
      | otherwise = do
          m <- V.read v i
          let c' = if m then c+1 else c
          count v c' (i+1)

