-- | Haskollider.Server defines all the types and functionals relavent to communicating with a running scsynth instances
module Haskollider.Server where

-- | Imports
import Haskollider.NetAddr
import Haskollider.Engine
import Haskollider.Node
import Haskollider.Util
import Haskollider.Buffer
import Sound.OSC
import Control.Monad.State
import Control.Concurrent
import Data.Maybe

-- | Data types

data Protocol = Tcp | Udp deriving (Eq, Show)

-- ServerType, distinguishes between an internal server or local host scsynth instance. LocalHost by default
data ServerType = LocalHost | InternalServer deriving(Eq, Show)

-- ServerOptions, a group of options passed to the server on creation.
data ServerOptions = ServerOptions {
	sNumAudioBusChannels :: Int,
	sNumControlBusChannels :: Int,
	sMaxLogins :: Int,
	sMaxNodes :: Int,
	sNumInputBusChannels :: Int,
	sNumOutputBusChannels :: Int,
	sNumBuffers :: Int,
	sMaxSynthDefs :: Int,
	sProtocol :: Protocol,
	sBufLength :: Int,
	sNumRGens :: Int,
	sMaxWireBufs :: Int,
	sPreferredSampleRate :: Int,
	sLoadGraphDefs :: Bool,
	sVerbosity :: Int,
	sRendezvous :: Bool,
	sRemoteControlVolume :: Bool,
	sMemoryLocking :: Bool,
	sPreferredHardwareBufferFrameSize :: Int,
	sRealTimeMemorySize :: Int,
	sBlockSize :: Int,
	sPortNum :: Int,
	sNumPrivateAudioBusChannels :: Int
} deriving (Show)

firstPrivateBus :: ServerOptions -> Int
firstPrivateBus o = (sNumOutputBusChannels o) + (sNumOutputBusChannels o)

-- A default collection of ServerOptions for quick use and reference
defaultServerOptions :: ServerOptions
defaultServerOptions = ServerOptions { 
	sNumAudioBusChannels = 128,
	sNumControlBusChannels = 4096,
	sMaxLogins = 64,
	sMaxNodes = 1024,
	sNumInputBusChannels = 8,
	sNumOutputBusChannels = 8,
	sNumBuffers = 1026,
	sMaxSynthDefs = 1024,
	sProtocol = Udp,
	sBufLength = 64,
	sNumRGens = 64,
	sMaxWireBufs = 64,
	sPreferredSampleRate = 44100,
	sLoadGraphDefs = True,
	sVerbosity = 0,
	sRendezvous = False,
	sRemoteControlVolume = False,
	sMemoryLocking = False,
	sPreferredHardwareBufferFrameSize = 512,
	sRealTimeMemorySize = 81920, -- Increased
	sBlockSize = 512,
	sPortNum = 57110,
	sNumPrivateAudioBusChannels = 112
}

-- Server, the main type used to instantiate and communicate with an scsynth instance
data Server = Server {
	serverName :: String,
	serverNetAddr :: NetAddr,
	serverOptions :: ServerOptions,
	serverClientID :: Int,
	serverRunning :: Bool,
	serverBootNotifyFirst :: Bool,
	serverLatency :: Double,
	serverDumpMode :: Int,
	serverNotify :: Bool,
	serverNotified :: Bool,
	serverNumUgens :: Int,
	serverNumSynths :: Int,
	serverAlive :: Bool,
	serverAliveThreadPeriod :: Double,
	serverRecHeaderFormat :: String,
	serverRecSampleFormat :: String,
	serverRecChannels :: Int,
	currentNodeID :: Int,
	controlBusAllocator :: PowerOfTwoAllocator,
	audioBusAllocator :: PowerOfTwoAllocator,
	bufferAllocator :: PowerOfTwoAllocator,
	rootNode :: Group,
	defaultGroup :: Group

} deriving (Show)

defaultServer :: Server
defaultServer = Server {
	serverName = "Local Host",
	serverNetAddr = newNetAddr "127.0.0.1" 57110,
	serverOptions = defaultServerOptions,
	serverClientID = 0,
	serverRunning = False,
	serverBootNotifyFirst = False,
	serverLatency = 0.2,
	serverDumpMode = 0,
	serverNotify = True,
	serverNotified = False,
	serverNumUgens = 0,
	serverNumSynths = 0,
	serverAlive = False,
	serverAliveThreadPeriod = 0.7,
	serverRecHeaderFormat = "wav",
	serverRecSampleFormat = "float",
	serverRecChannels = 2,
	currentNodeID = initialNodeID,
	controlBusAllocator = newPowerOfTwoAllocator (sNumControlBusChannels defaultServerOptions),
	audioBusAllocator = newPowerOfTwoAllocator (sNumAudioBusChannels defaultServerOptions),
	bufferAllocator = newPowerOfTwoAllocator (sNumBuffers defaultServerOptions),
	rootNode = newGroup rootNodeID, -- If you've already started scsynth from Supercollider these will be created by default
	defaultGroup = newGroup defaultGroupID
}

data ServerCommand = None |	Notify| Status | Quit | Cmd | D_recv | D_load | D_loadDir | D_freeAll | S_new | N_trace | N_free | N_run | N_cmd | N_map | 
					 N_set | N_setn | N_fill | N_before | N_after | U_cmd | G_new | G_head | G_tail | G_freeAll | C_set | C_setn | C_fill | B_alloc |
					 B_allocRead | B_read | B_write | B_free | B_close | B_zero | B_set | B_setn | B_fill | B_gen | DumpOSC | C_get | C_getn | B_get |
					 B_getn | S_get | S_getn | N_query | B_query | N_mapn | S_noid | G_deepFree | ClearSched | Sync | D_free | B_allocReadChannel |
					 B_readChannel | G_dumpTree | G_queryTree | Error | S_newargs | N_mapa | N_mapan | N_order | NUMBER_OF_COMMANDS deriving (Show, Eq, Ord, Enum)

-- | Internal functions, don't need to use these unless you have a good reason

sendMsg :: Server -> Message -> IO ()
sendMsg s m = sendNetAddrMsg (serverNetAddr s) m

sendBundle :: Server -> Bundle -> IO ()
sendBundle s b = sendNetAddrBundle (serverNetAddr s) b

nextNodeID :: StateT Server IO NodeID
nextNodeID = do
	server <- get
	put (server { currentNodeID = newID server})
	return $ newID server
		where newID s = allocNodeID (currentNodeID s)

allocControlBus :: Int -> StateT Server IO (Maybe Int)
allocControlBus n = do
	server <- get
	put (server { controlBusAllocator = snd $ cb server })
	return . fst $ cb server
		where cb s = allocPTBlock (controlBusAllocator s) n

freeControlBus :: Int -> StateT Server IO ()
freeControlBus n = do
	server <- get
	put (server { controlBusAllocator = fc server })
	return ()
		where fc s = freePTBlock (controlBusAllocator s) n 

allocAudioBus :: Int -> StateT Server IO (Maybe Int)
allocAudioBus n = do
	server <- get
	put (server { audioBusAllocator = snd $ ab server })
	return . fst $ ab server
		where ab s = allocPTBlock (audioBusAllocator s) n

freeAudioBus :: Int -> StateT Server IO ()
freeAudioBus n = do
	server <- get
	put (server { audioBusAllocator = fa server })
	return ()
		where fa s = freePTBlock (audioBusAllocator s) n

allocBuffer :: Int -> StateT Server IO (Maybe Int)
allocBuffer n = do
	server <- get
	put (server { bufferAllocator = snd $ bb server })
	return . fst $ bb server
		where bb s = allocPTBlock (bufferAllocator s) n

freeBuffer :: Int -> StateT Server IO ()
freeBuffer n = do
	server <- get
	put (server { bufferAllocator = fb server })
	return ()
		where fb s = freePTBlock (bufferAllocator s) n

-- | Below is the standard interface for using the Haskollider library.

-- Helper function, just wraps up the get and lift for slightly more streamlined sending. Used for things like set and free
send :: Message -> StateT Server IO ()
send m = do
	s <- get
	lift (sendMsg s m)

-- sendNew is a helper function for simplifying new node message sending to scsynth. It takes a function (NodeId to Node a), increments the node id
-- in the server, and returns the created Node. Eg sendNew $ newSynth "mySynth" [("arg1", value)] ... or also: sendNew $ newGroup
sendNew :: ScSendable a => (Int -> a) -> StateT Server IO a
sendNew f = do nextNodeID >>= (\i -> let n = f i in do (send $ newMsg n); return n)

sendSynth :: (Int -> Synth) -> StateT Server IO Synth
sendSynth = sendNew

sendGroup :: (Int -> Group) -> StateT Server IO Group
sendGroup = sendNew

sendBuffer :: (Int -> Buffer) -> StateT Server IO (Maybe Buffer)
sendBuffer f = do (allocBuffer 1) >>= (\i -> let b = buf i in do sendBuf b; return b)
	where
		buf (Just i) = Just $ f i
		buf Nothing = Nothing
		sendBuf (Just b) = send $ newMsg b
		sendBuf Nothing = do lift $ print "No more buffer numbers -- free some buffers before allocating more."; return ()
{- 
	Test code for Haskollider. First start SC and boot the server. Next, you'll need a Synth named "TestSine"; Something like this:
	
	SynthDef.new("TestSine", {
		arg freq = 440, amp = 0.2;
		var env = EnvGen.ar(Env.perc(0, 0.5), doneAction: 2);
		Out.ar(0, SinOsc.ar(freq, 0 , env * amp).dup);
	}).store;
	
	Next, make sure you have Haskollider compiled and installed (cabal build && cabal install). Finally, open up ghci and run
	:m Haskollider.Server
	testSC
-}

testSC :: IO ()
testSC = let 
	server = defaultServer
	synth f = sendSynth $ newSynth "TestSine" [("freq", fromIntegral f)] 	
	synths fundamental n = do
		synth $ fundamental * (mod n 8 + 1)
		synth $ fundamental * (mod n 7 + 1)
		lift $ threadDelay 100000
		synths fundamental (n + 1)

	in evalStateT (synths 80 1) server