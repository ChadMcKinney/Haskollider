-- | Haskollider.Bus defines the OSC message composition functions for controlling busses in scsynth
module Haskollider.Bus where

-- | Imports
import Sound.OSC

data Rate = Control | Audio deriving (Eq, Show, Enum)

data Bus = Bus { rate :: Rate, index :: Int, numChannels :: Int }

isSettable :: Bus -> Bool
isSettable bus = (rate bus) /= Audio

set :: Bus -> Double -> Message
set bus value = Message "/c_set" [int32 (index bus), double value]

setn :: Bus -> [Double] -> Message
setn bus values = Message "/c_setn" $ [int32 (index bus), int32 (length values)] ++ (foldr (\x acc -> (double x) : acc) [] values)

setAt :: Bus -> Int -> [Double] -> Message
setAt bus offset values = Message "/c_set" $ [int32 (index bus + offset), int32 (length values)] ++ (foldr (\x acc -> (double x) : acc) [] values)

fill :: Bus -> Double -> Int -> Message
fill bus value nChannels = Message "/c_fill" [int32 (index bus), int32 nChannels, double value]

setAll :: Bus -> Double -> Message
setAll bus value =  fill bus value (numChannels bus)

setValue :: Bus -> Double -> Message
setValue = setAll

newBus :: Rate -> Int -> (Int -> Bus)
newBus r n = bFunc
	where 
		bFunc i = Bus r i n







