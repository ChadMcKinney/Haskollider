module Main where

import Haskollider.NetAddr
import Haskollider.Engine
import Haskollider.Node
import Haskollider.Util
import Sound.OSC
import Control.Monad.State
import Control.Concurrent
import Data.Maybe
import Haskollider.Server

{-
	Test code for Haskollider. First start SC and boot the server. Next, you'll need a Synth named "TestSine"; Something like this:
	SynthDef.new("TestSine", {
		arg freq = 440, amp = 0.2;
		var env = EnvGen.ar(Env.perc(0, 0.5), doneAction: 2);
		Out.ar(0, SinOsc.ar(freq, 0 , env * amp).dup);
	}).store;
-}

main :: IO ()
main = let 
	server = defaultServer
	synth f = sendNode $ newSynth "TestSine" [("freq", fromIntegral f)] 	
	synths fundamental n = do
		synth $ fundamental * (mod n 8 + 1)
		synth $ fundamental * (mod n 7 + 1)
		lift $ threadDelay 100000
		synths fundamental (n + 1)

	in evalStateT (synths 80 1) server