-- | Haskollider.Osc facilitates the actual osc communication with a running scsynth instance.
module Haskollider.Osc where

-------------
-- | Imports

import Control.Concurrent
import Sound.OSC
import Control.Monad

----------------
-- | Data types

createOscServer :: Int -> (Message -> IO ()) -> IO ThreadId
createOscServer listeningPort parseFunc = forkIO $ withTransport server f
	where
		server = udpServer "127.0.0.1" listeningPort
		f = forever loop
		loop = do
			receivedMessage <- recvMessage
			case receivedMessage of
				Nothing -> return ()
				Just m -> liftIO $ parseFunc m
			liftIO $ print receivedMessage

createOscClient :: String -> Int -> IO UDP
createOscClient ip sendingPort = openUDP ip sendingPort

sendOsc :: IO UDP -> Message -> IO ()
sendOsc client oscMessage = withTransport client (sendMessage oscMessage)

sendOscBundle :: IO UDP -> Bundle -> IO()
sendOscBundle client oscBundle = withTransport client (sendBundle oscBundle)

{-
loginMessage :: Message
loginMessage = Message "/login" []

checkAddr :: Message -> Connection UDP ()
checkAddr (Message address datum)
       | address == "/login" = liftIO $ print "LOGIN! login"
       | otherwise = liftIO $ print "Error: Unrecognized message address pattern."

serverLoop :: Int -> String -> IO ()
serverLoop serverPort serverPassword = void $ withTransport t f
	where
		t = udpServer "127.0.0.1" serverPort
		f = forever loop
		loop = do
			receivedMessage <- recvMessage
			case receivedMessage of 
				Nothing -> liftIO $ print "Null message received."
				Just m -> checkAddr m        
			liftIO $ print receivedMessage

clientLoop :: String -> Int -> String -> IO ()
clientLoop serverIP serverPort serverPassword = withTransport t f
	where
		t = openUDP serverIP serverPort
		f = sendMessage loginMessage

-}

----------------
-- | Functions

parseIncomingOsc :: Message -> IO ()
parseIncomingOsc receivedMessage = print ("OSC: " ++ (show receivedMessage))