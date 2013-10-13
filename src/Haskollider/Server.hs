-- |Haskollider.Server defines all the types and functionals relavent to communicating with a running scsynth instances
module Haskollider.Server where

import Haskollider.NetAddr
import Haskollider.Engine
import Haskollider.Node
import Haskollider.Util
import Haskollider.Buffer
import qualified Haskollider.Bus as Bus
import Sound.OSC
import Control.Monad.State
import Control.Concurrent
import Data.Maybe
import Data.Time
import Data.List
import System.Process
import System.IO
import System.Posix.Env

-- |Protocol to use for Osc communication with scsynth. Udp is the default for server communication.
data Protocol = Tcp | Udp deriving (Eq, Show)

-- |ServerType, distinguishes between an internal server or local host scsynth instance. LocalHost by default
data ServerType = LocalHost | InternalServer deriving(Eq, Show)

-- |ServerProcess is just binds together the various handles returned from createProcess
data ServerProcess = ServerProcess { stdIn :: Handle, stdOut :: Handle, stdErr :: Handle, process :: ProcessHandle }

-- |ServerProcess instance of show, it simply displays a booted status.
instance Show ServerProcess where
	show (ServerProcess _ _ _ _) = "ServerProcess: Booted"

-- |ServerOptions, a group of options passed to the server on creation.
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

{-| 
  A default collection of ServerOptions for quick use and reference. 
  If you want to target specific arguments start here and change via record syntax.
-}
defaultServerOptions :: ServerOptions
defaultServerOptions = ServerOptions { 
	sNumAudioBusChannels = 128,
	sNumControlBusChannels = 4096,
	sMaxLogins = 64,
	sMaxNodes = 1024,
	sNumInputBusChannels = 8,
	sNumOutputBusChannels = 8,
	sNumBuffers = 1024,
	sMaxSynthDefs = 2048,
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
	sPortNum = 67110, -- Don't use the default SuperCollider scsynth port to prevent clashes
	sNumPrivateAudioBusChannels = 112
}

-- |Server, the main type used to instantiate and communicate with an scsynth instance
data Server = Server {
	serverName :: String,
	serverOptions :: ServerOptions,
	serverNetAddr :: NetAddr,
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
	defaultGroup :: Group,
	recordBuf :: Maybe Buffer,
	recordNode :: Maybe Synth,
	serverCmd :: String,
	serverProcess :: Maybe ServerProcess
} deriving (Show)

-- |A Server with typical values.
defaultServer :: Server
defaultServer = Server {
	serverName = "Local Host",
	serverOptions = defaultServerOptions,
	serverNetAddr = newNetAddr "127.0.0.1" (sPortNum defaultServerOptions),
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
	defaultGroup = newGroup defaultGroupID,
	recordBuf = Nothing,
	recordNode = Nothing,
	serverCmd = "scsynth",
	serverProcess = Nothing
}

data ServerCommand = None |	Notify| Status | Quit | Cmd | D_recv | D_load | D_loadDir | D_freeAll | S_new | N_trace | N_free | N_run | N_cmd | N_map | 
					 N_set | N_setn | N_fill | N_before | N_after | U_cmd | G_new | G_head | G_tail | G_freeAll | C_set | C_setn | C_fill | B_alloc |
					 B_allocRead | B_read | B_write | B_free | B_close | B_zero | B_set | B_setn | B_fill | B_gen | DumpOSC | C_get | C_getn | B_get |
					 B_getn | S_get | S_getn | N_query | B_query | N_mapn | S_noid | G_deepFree | ClearSched | Sync | D_free | B_allocReadChannel |
					 B_readChannel | G_dumpTree | G_queryTree | Error | S_newargs | N_mapa | N_mapan | N_order | NUMBER_OF_COMMANDS deriving (Show, Eq, Ord, Enum)

-------------------------------------------------------------------------------
-- Internal functions, don't need to use these unless you have a good reason
-------------------------------------------------------------------------------

-- |Helper function to find the first private bus
firstPrivateBus :: ServerOptions -> Int
firstPrivateBus o = (sNumOutputBusChannels o) + (sNumOutputBusChannels o)

-- |Converts ServerOptions to a list of command line Arguments for scsynth
optionsToCmdString :: ServerOptions -> [String]
optionsToCmdString options = udpP ++ numAudio ++ numControl ++ numInput ++ numOutput ++ blockS ++ hardBufSize ++ 
							 sRate ++ nBuffers ++ mNodes ++ mSDefs ++ rtMem ++ wBuf ++ logins
	where
		udpP = ["-u ", (show $ sPortNum options)]
		numAudio = ["-a ", (show $ sNumAudioBusChannels options)]
		numControl = ["-c ", (show $ sNumControlBusChannels options)]
		numInput = ["-i ", (show $ sNumInputBusChannels options)]
		numOutput = ["-o ", (show $ sNumOutputBusChannels options)]
		blockS = ["-z ", (show $ sBlockSize options)]
		hardBufSize = ["-Z ", (show $ sPreferredHardwareBufferFrameSize options)]
		sRate = ["-S ", (show $ sPreferredSampleRate options)]
		nBuffers = ["-b ", (show $ sNumBuffers options)]
		mNodes = ["-n ", (show $ sMaxNodes options)]
		mSDefs = ["-d ", (show $ sMaxSynthDefs options)]
		rtMem = ["-m ", (show $ sRealTimeMemorySize options)]
		wBuf = ["-w ", (show $ sMaxWireBufs options)]
		logins = ["-l ", (show $ sMaxLogins options)]

-- |Helper function for sending OSC messages to scsynth
sendMsg :: Server -> Message -> IO ()
sendMsg s m = sendNetAddrMsg (serverNetAddr s) m

-- |Helper function for sending OSC bundles to scsynth
sendBundle :: Server -> Bundle -> IO ()
sendBundle s b = sendNetAddrBundle (serverNetAddr s) b

-- |Increments the server's node counter, returning a node id.
nextNodeID :: StateT Server IO NodeID
nextNodeID = do
	server <- get
	put (server { currentNodeID = newID server})
	return $ newID server
		where newID s = allocNodeID (currentNodeID s)

-- |Allocates n number of control buses on the server
allocControlBus :: Int -> StateT Server IO (Maybe Int)
allocControlBus n = do
	server <- get
	put (server { controlBusAllocator = snd $ cb server })
	return . fst $ cb server
		where cb s = allocPTBlock (controlBusAllocator s) n

-- |Frees a control bus, identified by number
freeControlBus :: Int -> StateT Server IO ()
freeControlBus n = do
	server <- get
	put (server { controlBusAllocator = fc server })
	return ()
		where fc s = freePTBlock (controlBusAllocator s) n 

-- |Allocates n number of audio buses on the server
allocAudioBus :: Int -> StateT Server IO (Maybe Int)
allocAudioBus n = do
	server <- get
	put (server { audioBusAllocator = snd $ ab server })
	return . fst $ ab server
		where ab s = allocPTBlock (audioBusAllocator s) n

-- |Frees an audio bus, identified by number
freeAudioBus :: Int -> StateT Server IO ()
freeAudioBus n = do
	server <- get
	put (server { audioBusAllocator = fa server })
	return ()
		where fa s = freePTBlock (audioBusAllocator s) n

-- |Allocates n number of buffers on the server
allocBuffer :: Int -> StateT Server IO (Maybe Int)
allocBuffer n = do
	server <- get
	put (server { bufferAllocator = snd $ bb server })
	return . fst $ bb server
		where bb s = allocPTBlock (bufferAllocator s) n

-- |Frees a buffer, identified by number
freeBuffer :: Int -> StateT Server IO ()
freeBuffer n = do
	server <- get
	put (server { bufferAllocator = fb server })
	return ()
		where fb s = freePTBlock (bufferAllocator s) n

-- |Create the default group on the scsynth server. This is useful if you're starting it from the cml or otherwise not from sclang.
sendDefaultGroup :: Server -> IO ()
sendDefaultGroup server = sendMsg server $ Message "/g_new" [int32 defaultGroupID, int32 (fromEnum AddToHead), int32 rootNodeID]

-- |Process the stdout and stderr from the running scsynth instance
scprocessOut :: ServerProcess -> StateT Server IO (ThreadId)
scprocessOut scProcess = do
	server <- get
	lift . forkIO $ outFunc server
	where outFunc server = do
		outContents <- hGetContents $ stdOut scProcess
		errContents <- hGetContents $ stdErr scProcess

		if(isInfixOf "SuperCollider 3 server ready." outContents) 
			then do
				sendMsg server notify
				sendDefaultGroup server
				return ()
			else return ()

		putStrLn outContents
		putStrLn errContents
		return ()

-- |A simple Osc Message that when sent will ask scsynth to quit
quitMsg :: Message
quitMsg = Message "/quit" []

-------------------------------------------------------------------------
-- Below is the standard interface for using the Haskollider library.
-------------------------------------------------------------------------

-- |Helper function, just wraps up the get and lift for slightly more streamlined sending. Used for things like set and free
send :: Message -> StateT Server IO ()
send m = do
	s <- get
	lift (sendMsg s m)

-- |Takes a newBus function, allocates a bus, and returns a Bus object
sendBus :: Bus.Rate -> (Int -> Bus.Bus) -> StateT Server IO (Maybe Bus.Bus)
sendBus r f = do (allocBus 1) >>= (\i -> return (bus i))
	where
		allocBus = if r == Bus.Audio then (allocAudioBus) else (allocControlBus) 
		bus (Just i) = Just $ f i
		bus Nothing = Nothing

{-| 
  sendNew is a helper function for simplifying new node message sending to scsynth. It takes a function (NodeId to Node a), increments the node id
  in the server, and returns the created Node. Eg sendNew $ newSynth "mySynth" [("arg1", value)] ... or also: sendNew $ newGroup
-}
sendNew :: ScSendable a => (Int -> a) -> StateT Server IO a
sendNew f = do nextNodeID >>= (\i -> let n = f i in do (send $ newMsg n); return n)

-- |A Synth type based wrapper for sendNew
sendSynth :: (Int -> Synth) -> StateT Server IO Synth
sendSynth = sendNew

-- |A Group type based wrapper for sendNew
sendGroup :: (Int -> Group) -> StateT Server IO Group
sendGroup = sendNew

{-| 
  Similar to sendNew, this takes a curried function (returned by various Buffer module functions) and returns a Buffer  
  When executed a buffer is allocated on the server, passed functions takes that number and returns a function which is sent to the server
-}
sendBuffer :: (Int -> Buffer) -> StateT Server IO (Maybe Buffer)
sendBuffer f = do (allocBuffer 1) >>= (\i -> let b = buf i in do sendBuf b; return b)
	where
		buf (Just i) = Just $ f i
		buf Nothing = Nothing
		sendBuf (Just b) = send $ newMsg b
		sendBuf Nothing = do lift $ print "No more buffer numbers -- free some buffers before allocating more."; return ()

-- |Requests scsynth to load a directory of compiled synth definitions
loadDirectory :: String -> StateT Server IO ()
loadDirectory dir = send $ Message "/d_loadDir" [string dir]

-- |Begin recording, this just uses the current working directory and a generated filename based on the current time.
record :: StateT Server IO ()
record = do
	prepareForRecording
	server <- get
	if isNothing (recordNode server) 
		then do
			rNode <- sendSynth $ synthTail (rootNode server) "server-record" [("bufnum", fromIntegral . bufnum . fromJust $ recordBuf server)]
			server' <- get
			put (server' { recordNode = Just rNode })
		else send $ run (fromJust $ recordNode server) True

-- |Pauses the current recording, if any.
pauseRecording :: StateT Server IO ()
pauseRecording = do
	server <- get
	if isJust (recordNode server) 
		then send $ run (fromJust $ recordNode server) False 
		else lift $ print "Warning: Not recording"

-- |Stops and finalized the current recording, if any.
stopRecording :: StateT Server IO ()
stopRecording = do
	server <- get
	if isJust (recordNode server) 
		then do
			send $ freeNode (fromJust $ recordNode server)
			send $ close (fromJust $ recordBuf server)
			send $ freeBuf (fromJust $ recordBuf server)
			put (server { recordNode = Nothing, recordBuf = Nothing })
		else lift $ print "Warning: Not recording"

-- |A helper function for created the necessary node and buffer for record. Not necessary to call this before recording.
prepareForRecording :: StateT Server IO ()
prepareForRecording = do
	server <- get
	if isJust (recordBuf server) then return () else do
		rBuf <- sendBuffer $ alloc 65536 (serverRecChannels server)
		now <- lift getCurrentTime
		case rBuf of
			Nothing -> return ()
			Just buf -> do
				lift $ putStrLn ("prepareForRecording path: " ++ newPath)
				send $ write buf newPath (serverRecHeaderFormat server) (serverRecSampleFormat server) 0 0 True
					where newPath = filter (/=' ') ("SC_" ++ (show now) ++ "." ++ (serverRecHeaderFormat server))

		put (server { recordBuf = rBuf }) 

-- |Boots the server, spawning a child scsynth process and established pipes for stdout and sterr
boot :: StateT Server IO ()
boot = do
	lift $ putEnv "SC_JACK_DEFAULT_INPUTS=system"
	lift $ putEnv "SC_JACK_DEFAULT_OUTPUTS=system"
	-- automatically start jack when booting the server
	-- can still be overridden with JACK_NO_START_SERVER
	lift $ putEnv "JACK_START_SERVER=true"
	server <- get
	(mstin, mstout, msterr, pHandle) <- lift $ 
		createProcess (proc "scsynth" (optionsToCmdString $ serverOptions server))  { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
	let stList = [mstin, mstout, msterr]
	if (any (isNothing) stList) 
		then do 
			lift $ putStrLn "NOTHING!"
			return ()
		else case stList of
			[Just stin, Just stout, Just sterr] -> let
				scProcess = ServerProcess stin stout sterr pHandle
				in do
				put (server { serverProcess = Just scProcess, serverRunning = True })	
				scprocessOut scProcess
				return ()

-- |Quits the child scsynth process
quit :: StateT Server IO ()
quit = do
	send quitMsg
	server <- get
	case (serverProcess server) of
		Nothing -> return ()
		Just scProcess -> do
			lift . hClose $ stdIn scProcess
			lift . hClose $ stdOut scProcess
			lift . hClose $ stdErr scProcess
			lift $ waitForProcess (process scProcess)
			put (server { serverRunning = False })
			return ()

-- |A notify message, when notified scsynth will start sending update messages to sender regarding running synths, ugens, etc.
notify :: Message
notify = Message "/notify" [int32 1]

{-| 
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
		if n < 200
			then synths fundamental (n + 1)
			else return()

	in evalStateT (synths 80 1) server

{-| 
	More test code for Haskollider. First start SC and boot the server. Next, you'll need a Synth named "TestSine2"; Something like this:
	
	SynthDef.new("TestSine2", {
		arg freq = 440, amp = 0.2, gate = 1;
		var env = EnvGen.ar(Env.asr(0, 1, 0.5), gate: gate, doneAction: 2);
		Out.ar(0, SinOsc.ar(freq, 0 , env * amp).dup);
	}).store;
	
	Next, make sure you have Haskollider compiled and installed (cabal build && cabal install). Finally, open up ghci and run
	:m Haskollider.Server
	testSC2
-}
testSC2 :: IO ()
testSC2 = let 
	server = defaultServer
	synth f = sendSynth $ newSynth "TestSine2" [("freq", f)]
	
	runSynths = do
		synth1 <- synth 160.0
		synth2 <- synth 160.0
		synths synth1 synth2 80 1

	synths :: Synth -> Synth -> Double -> Int -> StateT Server IO ()
	synths synth1 synth2 fundamental n = do
		send $ set synth1 "freq" (fundamental * fromIntegral (mod n 9 + 1))
		send $ set synth2 "freq" (fundamental * fromIntegral (mod n 7 + 1))
		lift $ threadDelay 100000
		if n < 1000
			then synths synth1 synth2 fundamental (n + 4)
			else do
				send $ set synth1 "gate" 0
				send $ set synth2 "gate" 0

	in evalStateT (runSynths) server

{-|
	Server Process control test. Using the boot/quit functions we don't have to start SuperCollider outside of the program.
	This assumes you're on a unixy system with supercollider installed and with scsynth in your path.
	Also, for recording to work you need to have a "server-record" synth def like such compiled:
	
	SynthDef("server-record", { arg bufnum;
		DiskOut.ar(bufnum, In.ar(0, recChannels))
	}).store;
-}
testSC3 :: IO ()
testSC3 = evalStateT (runSC) server
	where
		server = defaultServer
		synth f = sendSynth $ newSynth "TestSine" [("freq", fromIntegral f)] 	
		synths fundamental n = do
			synth $ fundamental * (mod n 8 + 1)
			synth $ fundamental * (mod n 7 + 1)
			lift $ threadDelay 100000
			if n < 200
				then synths fundamental (n + 1)
				else return()
		runSC :: StateT Server IO ()
		runSC = do
			boot
			lift $ threadDelay 1000000
			record
			synths 80 1
			stopRecording
			quit


