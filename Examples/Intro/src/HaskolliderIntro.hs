module Main where

import Control.Concurrent

import Sound.OSC
import Network.Socket.Internal

import Control.Monad 

import Haskollider
import Haskollider.Server

main :: IO ()
main = testSC


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


--}