{-# LANGUAGE FlexibleContexts, BangPatterns #-}
-- Modified from
--- code by Simon Marlow for his book 'Parallel & Concurrent Haskell

import Data.Array.Repa as R hiding ((++))
import Data.Array.Repa.IO.DevIL
import System.Environment
import Data.Array.Repa.Repr.ForeignPtr
import Data.Word
import System.Directory
import Control.Monad
import Control.Monad.IO.Class

-- <<main
main :: IO ()
main = do
    [deg, red, f1,f2] <- getArgs
    runIL $ do
      (RGB v) <- readImage f1                                            
      let rotated = rotate (read deg) (delay v) :: (Array D DIM3 Word8)
      let reddened = redden (read red) rotated :: (Array D DIM3 Word8)
      final <- computeP reddened
      exists <- liftIO$ doesFileExist f2
      when exists $ liftIO $ removeFile f2
      writeImage f2 (RGB final)                                        

{-# INLINE redden #-}
redden :: Double -> Array D DIM3 Word8 -> Array D DIM3 Word8
redden !red !g 
  = R.traverse g id (\ix sh@(Z:.y:.x:.k) ->
                              if k==0 then
                                reddenw (ix sh)
                              else 
                                (ix sh))
  where
    reddenw::Word8 -> Word8
    reddenw w = fina
      where
        init = fromIntegral w :: Double
        satur = fromIntegral (maxBound::Word8) :: Double
        incr = init*(1+red/100)
        fina = round (min incr satur)

{-# INLINE rotate #-}
rotate :: Double -> Array D DIM3 Word8 -> Array D DIM3 Word8
rotate !deg !g = R.traverse g id f
    where
        sh@(Z :. y :. x :. k)   = extent g

        !theta = pi/180 * deg                         

        !st = sin theta                               
        !ct = cos theta

        !cy = fromIntegral y / 2 :: Double            
        !cx = fromIntegral x / 2 :: Double

        f ix (Z :. i :. j :. k)                          
          | inShape sh old = ix old                  
          | otherwise      = 0                        
          where
            fi = fromIntegral i - cy                  
            fj = fromIntegral j - cx

            i' = round (st * fj + ct * fi + cy)       
            j' = round (ct * fj - st * fi + cx)

            old = Z :. i' :. j' :. k                  


