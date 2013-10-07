module Haskollider.NetAddr where

import Haskollider.Osc
import Sound.OSC

data NetAddr = NetAddr { netAddrHostName :: String, netAddrPortNum :: Int, oscClient :: (IO UDP) }

instance Show NetAddr where
	show (NetAddr ip port _) = "host: " ++ ip ++ ", port: " ++ (show port)

-- Use this for conveniance
newNetAddr :: String -> Int -> NetAddr
newNetAddr ip port = NetAddr ip port (createOscClient ip port)

sendNetAddrMsg :: NetAddr -> Message -> IO ()
sendNetAddrMsg n m = sendOsc (oscClient n) m

sendNetAddrBundle :: NetAddr -> Bundle -> IO()
sendNetAddrBundle n b = sendOscBundle (oscClient n) b