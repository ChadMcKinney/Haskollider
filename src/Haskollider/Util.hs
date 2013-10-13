-- |Utility functions for internal usage.
module Haskollider.Util where

import Control.Monad.State
import Debug.Trace
import Sound.OSC.Core

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