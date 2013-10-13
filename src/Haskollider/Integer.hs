-- |Inter module contins internal functions used for block allocation
module Haskollider.Integer where

import Data.Bits

log2Floor :: Int -> Int
log2Floor x = recurRes x 0
	where recurRes argX res = if shiftX > 0 then recurRes shiftX (res + 1) else res
		where shiftX = (shiftR argX 1) 

log2Ceil :: Int -> Int
log2Ceil x = if notPowerOf2 > 0 then (log2Floor x) + 1 else log2Floor x
	where notPowerOf2 = x .&. (x - 1)

nextPowerOfTwo :: Int -> Int
nextPowerOfTwo x = (+1) $ foldl (\acc n -> acc .|. (shiftR acc n)) (x - 1) [1,2,4,8,16] -- 32 bit integers only