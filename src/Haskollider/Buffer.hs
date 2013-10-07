-- | Haskollider.Buffer
module Haskollider.Buffer where

-- | Imports
import Sound.OSC
import Haskollider.Util

data Buffer = Buffer {
	numFrames :: Int,
	numChannels :: Int,
	bufnum :: Int,
	bMessage :: Message
} deriving (Show)

instance ScSendable Buffer where
	newMsg (Buffer _ _ _ msg) = msg

alloc :: Int -> Int -> (Int -> Buffer)
alloc frames channels = bFunc
	where
		bFunc n = Buffer frames channels n (msg n)
		msg = (\n -> Message "/b_alloc" [int32 n, int32 frames, int32 channels])

allocRead :: String -> Int -> Int -> (Int -> Buffer)
allocRead path startFrame frames = bFunc
	where
		bFunc n = Buffer frames 1 n (msg n)
		msg = (\n -> Message "/b_allocRead" [int32 n, string path, int32 startFrame, int32 frames])

allocReadChannel :: String -> Int -> Int -> Int -> (Int -> Buffer)
allocReadChannel path startFrame frames channels = bFunc
	where
		bFunc n = Buffer frames channels n (msg n)
		msg = (\n -> Message "/b_allocReadChannel" [int32 n, string path, int32 startFrame, int32 frames, int32 channels])

query :: Buffer -> Message
query b = Message "/b_query" [int32 (bufnum b)]

read :: Buffer -> String -> Int -> Int -> Bool -> Message
read buf path startFrame frames leaveOpen = Message "/b_read" [int32 (bufnum buf), string path, int32 startFrame, int32 frames, int32 (flag leaveOpen)]

readChannel :: Buffer -> String -> Int -> Int -> Bool -> Int -> Message
readChannel buf path startFrame frames leaveOpen channels = Message "/b_readChannel" args
	where args = [int32 (bufnum buf), string path, int32 startFrame, int32 frames, int32 (flag leaveOpen), int32 channels]

write :: Buffer -> String -> String -> String -> Int -> Int -> Bool -> Message
write buf path headerFormat sampleFormat startFrame frames leaveOpen = Message "/b_write" args
	where args = [int32 (bufnum buf), string path, string headerFormat, string sampleFormat, int32 frames, int32 startFrame, int32 (flag leaveOpen)] 

freeBuf :: Buffer -> Message
freeBuf buf = Message "/b_free" [int32 (bufnum buf)]

close :: Buffer -> Message
close buf = Message "/b_close" [int32 (bufnum buf)]

sendCollection :: Buffer -> [Double] -> Int -> Maybe [Message]
sendCollection buf collection startFrame = if (length collection) > (((numFrames buf) - startFrame) * (numChannels buf))
		then Nothing
		else Just $ foldl accumSamples [] [0,1..(numMessages-1)]
				where 
					maxSize = 513 -- max size for setn under udp
					numMessages = ceiling ((fromIntegral (length collection) :: Double) / fromIntegral maxSize)
					accumSamples acc pos = acc ++ [samplesMessage]
						where
							currentSample = (pos * maxSize)
							bundleSize = min maxSize ((length collection) - currentSample)
							samplesMessage = Message "/b_set" $ [int32 (bufnum buf)] ++ (foldl foldDatum [] [currentSample..(currentSample + (bundleSize - 1))])
								where foldDatum = (\acc2 i -> acc2 ++ [int32 i, float (collection !! i)])

sinList :: Double -> [Double]
sinList s = map (\x -> sin (x / s)) [1..s]

messageListToDoubleList :: Maybe [Message] -> Maybe [Double]
messageListToDoubleList mm = case mm of
	Nothing -> Nothing
	Just ms -> Just $ foldl (\acc (Message _ dm) -> acc ++ (mapMessage dm)) [] ms 
		where mapMessage m = map (\x -> let d = (d_get x) in checkD d) (dropN (2 :: Int) $ drop 2 m)-- drop 2 (first is bufnum, second is initial index, then we dropN)
			where checkD cd = case cd of
				Nothing -> 0
				Just d' -> d' 

-- testing our sendCollection function for correct behavior. It looks like we've got it working right!
testSendCollection :: Int -> IO (Bool)
testSendCollection testSize =  let
	testCollection = sinList (fromIntegral testSize)
	-- Not actually instancing on any server, this is just a dummy arg for the test
	testBuffer = Buffer testSize 1 0 (Message "/b_alloc" [int32 (0 :: Int), int32 testSize, int32 (1 :: Int)]) 
	sendCollectionMessages = sendCollection testBuffer testCollection 0
	translatedCollection = messageListToDoubleList sendCollectionMessages
	result = case translatedCollection of
		Nothing -> False
		Just sc -> testCollection == sc
	in do
	putStrLn "-----------------------------------------------"
	putStrLn "-- Beginning sendCollection Test"
	putStrLn "-----------------------------------------------"
	putStrLn " testCollection: "
	print testCollection
	putStrLn " translatedCollection: "
	print translatedCollection
	putStrLn "-----------------------------------------------"
	putStrLn "-- result: "
	putStrLn "-----------------------------------------------"

	return result