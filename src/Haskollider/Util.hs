-- |Utility functions for internal usage.
module Haskollider.Util where

import Control.Monad.State
import Debug.Trace
import Sound.OSC.Core
import Data.Bits
import Data.Int
import Data.Word
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Builder as BSB

-- |Helper function to convert a Bool to Int for use in server Osc Messages
flag :: Bool -> Int
flag b = case b of
	True -> 1
	False -> 0

-- |Drops m number of elements from a list
dropN :: (Enum b1, Eq b1, Num b1) => b1 -> [b] -> [b]
dropN m = map snd . filter ((== 1) . fst) . zip (cycle [1..m])

-- |Print function for state monads
traceState :: (Show a) => a -> State s a
traceState x = state (\s -> trace ("test: " ++ show x) (x, s))

-- |Print function for state monads, prints x
traceState2 :: (Show s) => a -> State s a
traceState2 x = state (\s -> trace ("test: " ++ show s) (x, s))

-- |Test print for state monads. Prints x == y
testState :: (Show a, Eq a) => a -> a -> State s a
testState x y = state (\s -> trace ("test: " ++ show (x == y)) (x, s))

-- |Class for all sendable types, instanced by Synth, Group, Bus, and Buffer
class ScSendable a where
	newMsg :: a -> Message

-- |Collect the values of a list from the given indexes. Safe lookup, won't break on out of bounds, will simply return any result who's index can be found in the 'is' list
valuesAt :: [Int] -> [a] -> [a]
valuesAt is x = map (\(x',_) -> x') . filter (\(_,i) -> i `elem` is) $ zip x [0..]	

-- |Bit twiddling
word8 :: (Integral a, Bits a) => a -> Word8
word8 word = (fromIntegral $ word .&. 0xFF) :: Word8

word16ToByteString :: Int16 -> BS.ByteString
word16ToByteString = BSB.toLazyByteString . BSB.word16BE . fromIntegral

word32ToByteString :: Int32 -> BS.ByteString
word32ToByteString = BSB.toLazyByteString . BSB.word32BE . fromIntegral

stringBytes :: String -> BS.ByteString
stringBytes = BSB.toLazyByteString . BSB.stringUtf8

-- |Print an array accross lines for easier reading
arrayPrint :: (Show a) => [a] -> IO ()
arrayPrint ar = do
	putStr "[\n"
	putStr . unlines . map (show) $ ar
	putStr "]\n"