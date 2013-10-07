-- | Haskollider.Node defines a set of functions for Osc message composition that allows for the creation and control of Nodes, Synths, and Groups in scsynth 
module Haskollider.Node where

-- | Imports
import Sound.OSC
import Haskollider.Engine
import Haskollider.Util

-- | AddAction is a simple enum used to organize nodes upon creation
data AddAction = AddToHead | AddToTail | AddBefore | AddAfter | AddReplace deriving (Ord, Eq, Enum)

type ControlPair = (String, Double)
type ControlList = [ControlPair]

rootNodeID :: NodeID
rootNodeID = 0

defaultGroupID :: NodeID
defaultGroupID = 1

grainNodeID :: NodeID
grainNodeID = -1

controlToDatum :: ControlList -> [Datum]
controlToDatum cl = (foldr (\(arg, val) acc -> string arg : float val : acc) [] cl)

-- Node is the abstract base class for groups and synths. Many functions are useful to any subclass such as free and moveToHead
class Node a where
	nodeID :: a -> NodeID

freeNode :: Node a => a -> Message
freeNode n = Message "/n_free" [int32 (nodeID n)]

run :: Node a => a -> Bool -> Message
run n b = Message "/n_run" [int32 (nodeID n), int32 (flag b)]

set :: Node a => a -> String -> Double -> Message
set n arg val = Message "/n_set" [int32 (nodeID n), string arg, float val]

setList :: Node a => a -> ControlList -> Message
setList n xs = Message "/n_set" $ int32 (nodeID n) : (controlToDatum xs)

setn :: Node a => a -> Int -> Int -> [Double] -> Message
setn n firstIndex numIndexes values = Message "/n_setn" $ [int32 (nodeID n), int32 firstIndex, int32 numIndexes ] ++ (map (float) values)

fill :: Node a => a -> Int -> Int -> Double -> Message
fill n firstIndex numIndexes value = Message "/n_fill" $ [int32 (nodeID n), int32 firstIndex, int32 numIndexes, float value]

release :: Node a => a -> Double -> Message
release n releaseTime = set n "gate" ((-1.0) - releaseTime)

trace :: Node a => a -> Message
trace n = Message "/n_trace" [int32 (nodeID n)]

moveBefore :: Node a => a -> a -> Message
moveBefore n n2 = Message "/n_before" [int32 (nodeID n), int32 (nodeID n2)]

moveAfter :: Node a => a -> a -> Message
moveAfter n n2 = Message "/n_after" [int32 (nodeID n), int32 (nodeID n2)]

moveToHead :: Node a => a -> Group -> Message
moveToHead n g = moveNodeToHead g n

moveToTail :: Node a => a -> Group -> Message
moveToTail n g = moveNodeToTail g n

-- | Group is the node subclass that defines an interface for organizing contained nodes
data Group = Group NodeID Message deriving (Show)

instance Node Group where
	nodeID (Group nID _ ) = nID

instance ScSendable Group where
	newMsg (Group _ msg) = msg

moveNodeToHead :: Node n => Group -> n -> Message
moveNodeToHead g n = Message "/g_head" [int32 (nodeID g), int32 (nodeID n)]

moveNodeToTail :: Node n => Group -> n -> Message
moveNodeToTail g n = Message "/g_tail" [int32 (nodeID g), int32 (nodeID n)]

freeAll :: Group -> Message
freeAll g = Message "/g_freeAll" [int32 (nodeID g)]

deepFree :: Group -> Message
deepFree g = Message "/g_deepFree" [int32 (nodeID g)]

dumpTree :: Group -> Message
dumpTree g = Message "/g_dumpTree" [int32 (nodeID g)]

newGroup :: (NodeID -> Group)
newGroup = gFunc
	where
		gFunc groupID = Group groupID (nMsg groupID) 
		nMsg = (\groupID -> Message "/g_new" [int32 groupID, int32 (fromEnum AddToHead), int32 defaultGroupID])

newGroup' :: Node a => a -> AddAction -> (NodeID -> Group)
newGroup' target action = gFunc
	where
		gFunc groupID = Group groupID (nMsg groupID) 
		nMsg = (\groupID -> Message "/g_new" [int32 groupID, int32 (fromEnum action), int32 $ nodeID target])

groupAfter :: Node a => a -> (NodeID -> Group)
groupAfter target = newGroup' target AddAfter

groupBefore :: Node a => a -> (NodeID -> Group)
groupBefore target = newGroup' target AddBefore

groupHead :: Group -> (NodeID -> Group)
groupHead target = newGroup' target AddToHead

groupTail :: Group -> (NodeID -> Group)
groupTail target = newGroup' target AddToTail

groupReplace :: Node a => a -> (NodeID -> Group)
groupReplace target = newGroup' target AddReplace

-- | Synth is a Node subclass for nodes that actual generate audio and control output in scsynth
data Synth = Synth String NodeID Message deriving (Show)

instance Node Synth where
	nodeID (Synth _ nID _ ) = nID

instance ScSendable Synth where
	newMsg (Synth _ _ msg) = msg

synthName :: Synth -> String
synthName (Synth sName _ _) = sName

newSynth :: String -> ControlList -> (NodeID -> Synth)
newSynth name cl = sFunc 
	where 
		sFunc synthID = Synth name  synthID (nMsg synthID)
		nMsg = (\synthID -> Message "/s_new" $ [string name, int32 synthID, int32 (fromEnum AddToHead), int32 defaultGroupID] ++ (controlToDatum cl))

newSynth' :: Node a => String -> ControlList -> a -> AddAction -> (NodeID -> Synth)
newSynth' name cl target action = sFunc 
	where 
		sFunc synthID = Synth name  synthID (nMsg synthID)
		nMsg = (\synthID -> Message "/s_new" $ [string name, int32 synthID, int32 (fromEnum action), int32 $ nodeID target] ++ (controlToDatum cl))

synthAfter :: Node a => a -> String -> ControlList -> (NodeID -> Synth)
synthAfter target name cl = newSynth' name cl target AddAfter

synthBefore :: Node a => a -> String -> ControlList -> (NodeID -> Synth)
synthBefore target name cl = newSynth' name cl target AddBefore

synthHead :: Group -> String -> ControlList -> (NodeID -> Synth)
synthHead target name cl = newSynth' name cl target AddToHead

synthTail :: Group -> String -> ControlList -> (NodeID -> Synth)
synthTail target name cl = newSynth' name cl target AddToTail

synthReplace :: Node a => a -> String -> ControlList -> (NodeID -> Synth)
synthReplace target name cl = newSynth' name cl target AddReplace

grainSynth :: String -> ControlList -> Message
grainSynth name cl = Message "/s_new" $ [string name, int32 grainNodeID, int32 (fromEnum AddToHead), int32 defaultGroupID] ++ (controlToDatum cl)

grainSynth' :: Node a => String -> ControlList -> a -> AddAction -> Message
grainSynth' name cl target action = Message "/s_new" $ [string name, int32 grainNodeID, int32 (fromEnum action), int32 $ nodeID target] ++ (controlToDatum cl)