module Haskollider.Util where

import Control.Monad.State
import Debug.Trace
import Sound.OSC.Core

flag :: Bool -> Int
flag b = case b of
	True -> 1
	False -> 0

dropN :: (Enum b1, Eq b1, Num b1) => b1 -> [b] -> [b]
dropN m = map snd . filter ((== 1) . fst) . zip (cycle [1..m])

traceState :: (Show a) => a -> State s a
traceState x = state (\s -> trace ("test: " ++ show x) (x, s))

traceState2 :: (Show s) => a -> State s a
traceState2 x = state (\s -> trace ("test: " ++ show s) (x, s))

testState :: (Show a, Eq a) => a -> a -> State s a
testState x y = state (\s -> trace ("test: " ++ show (x == y)) (x, s))

class ScSendable a where
	newMsg :: a -> Message