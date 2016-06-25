{-# LANGUAGE BangPatterns #-}
module Main (main) where

import System.Environment (getArgs)
import qualified Data.Vector.Unboxed as VI
import qualified Data.Vector.Unboxed.Mutable as V
import Control.Monad.ST
import Control.Monad

main::IO ()
main = do
  [a1] <- getArgs
  let n = read a1
  putStrLn $ "Count = "++show (countPrimes n)

countPrimes::Int->Int
countPrimes n 
  = VI.length $ VI.filter id $ mask
  where
    mask = VI.create mkMask
    mkMask::ST s (V.MVector s Bool)
    mkMask = do
      v <- V.replicate n True
      V.write v 0 False
      V.write v 1 False
      forM_ [2..(n-1)] $ \p -> do
        b <- V.read v p
        when b $ 
          forM_ [p*p,p*p+p..(n-1)] $ \i ->
            V.write v i False
      return v

