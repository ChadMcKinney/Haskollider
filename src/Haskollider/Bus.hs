-- |Haskollider.Bus defines the OSC message composition functions for controlling busses in scsynth
module Haskollider.Bus where

import Sound.OSC

-- |Bus rate algebraic type. Audio rate has a faster update rate.
data Rate = Control | Audio deriving (Eq, Show, Enum)

-- |Bus type for controlling buses in scsynth. Takes a rate, index, and number of channels
data Bus = Bus { rate :: Rate, index :: Int, numChannels :: Int }

isSettable :: Bus -> Bool
isSettable bus = (rate bus) /= Audio

set :: Bus -> Double -> Message
set bus value = Message "/c_set" [int32 (index bus), float value]

setn :: Bus -> [Double] -> Message
setn bus values = Message "/c_setn" $ [int32 (index bus), int32 (length values)] ++ (foldr (\x acc -> (float x) : acc) [] values)

setAt :: Bus -> Int -> [Double] -> Message
setAt bus offset values = Message "/c_set" $ [int32 (index bus + offset), int32 (length values)] ++ (foldr (\x acc -> (float x) : acc) [] values)

fill :: Bus -> Double -> Int -> Message
fill bus value nChannels = Message "/c_fill" [int32 (index bus), int32 nChannels, float value]

setAll :: Bus -> Double -> Message
setAll bus value =  fill bus value (numChannels bus)

setValue :: Bus -> Double -> Message
setValue = setAll

-- |Given a rate and number of channels, returns a function that takes an index and returns a bus. Use with sendBus
newBus :: Rate -> Int -> (Int -> Bus)
newBus r n = bFunc
	where 
		bFunc i = Bus r i n







