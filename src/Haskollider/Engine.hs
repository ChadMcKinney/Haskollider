module Haskollider.Engine where

-- | Imports
import Haskollider.Integer
import Debug.Trace
import qualified Data.Vector as V

-- | NodeID Allocation

type NodeID = Int

initialNodeID :: NodeID
initialNodeID = 1000

allocNodeID :: NodeID -> NodeID
allocNodeID n = (mod (n + 1) (maxBound :: Int))

resetNodeID :: NodeID
resetNodeID = initialNodeID

-- | Bus and Buffer Allocation

-- | PowerOfTwoBlockAllocator

data PowerOfTwoBlock = PowerOfTwoBlock { blockAddress :: Int, blockSize :: Int, blockNext :: Maybe PowerOfTwoBlock } deriving (Eq, Show)

instance Ord PowerOfTwoBlock where
	compare x y
         | (blockAddress x) == (blockAddress y)    =  EQ
         | (blockAddress x) <= (blockAddress y)    =  LT
         | otherwise =  GT

type BlockRegion =  V.Vector (Maybe PowerOfTwoBlock)

replaceBlockAt :: Int -> (Maybe PowerOfTwoBlock) -> BlockRegion -> BlockRegion
replaceBlockAt index value region = V.update region $ V.fromList [(index, value)]

data PowerOfTwoAllocator = PowerOfTwoAllocator { 
	allocatorSize :: Int,
	allocatorArray :: BlockRegion, 
	allocatorFreeList :: BlockRegion,
	allocatorPos :: Int
}

instance Show PowerOfTwoAllocator where
	show (PowerOfTwoAllocator s a f p) = "size: " ++ (show s) ++ ", arraySize: " ++ (show (V.length a)) ++ 
										 ", freeListSize: " ++ (show (V.length f)) ++ ", position: " ++ (show p)

newPowerOfTwoAllocator :: Int -> PowerOfTwoAllocator
newPowerOfTwoAllocator aSize = PowerOfTwoAllocator aSize (V.replicate aSize Nothing) (V.replicate 32 Nothing) 0

allocPTBlock :: PowerOfTwoAllocator -> Int -> ((Maybe NodeID), PowerOfTwoAllocator)
allocPTBlock pt n = let 
	n' = nextPowerOfTwo n
	sizeClass = log2Ceil n'
	getNode p = (allocatorFreeList p) V.! sizeClass

	in case (getNode pt) of
			Nothing -> if (address + n') <= (allocatorSize pt)
							then (Just address, pt { allocatorArray = newArray, allocatorPos = newPos })
							else (Nothing, pt)
							where 
								address = (allocatorPos pt)
								newArray = replaceBlockAt address (Just $ PowerOfTwoBlock address n' Nothing) (allocatorArray pt)
								newPos = (allocatorPos pt) + n'
			Just node -> (Just (blockAddress node), pt { allocatorFreeList = newFreeList })
				where newFreeList = replaceBlockAt sizeClass (blockNext node) (allocatorFreeList pt)

freePTBlock :: PowerOfTwoAllocator -> Int -> PowerOfTwoAllocator
freePTBlock pt address = case (allocatorArray pt) V.! address of
	Nothing -> pt
	Just node -> pt { allocatorArray = newArray, allocatorFreeList = newFreeList }
		where 
			sizeClass = log2Ceil (blockSize node)
			newArray = replaceBlockAt address Nothing (allocatorArray pt)
			newFreeList = replaceBlockAt sizeClass (Just node { blockNext = (allocatorFreeList pt) V.! sizeClass }) (allocatorFreeList pt)

potAllocUnitTest :: IO()
potAllocUnitTest = let 
		pt = newPowerOfTwoAllocator 32
		answers1 = [0,1,5,7,8,16,17,21]
		answers2 = [16,17,5,0,8,7,21,1] 
		test p l = foldl (\acc (x,y) -> let (n, pt') = (allocPTBlock acc x) in trace (show n ++ ", " ++ show (n == Just y)) pt') p $ zip [1,3,2,1,7,1,3,4] l
		test1 = test pt answers1
		test2 = foldl (\acc x -> let r = freePTBlock acc x in trace (show r) r) test1 [8,7,0,1,21,17,5,16]
		test3 = test test2 answers2
	in do print test3