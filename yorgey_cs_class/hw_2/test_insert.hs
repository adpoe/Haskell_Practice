module LogAnalysis where
import Log


insert :: LogMessage -> MessageTree -> MessageTree
insert msg tree 
	| findTimeMsg (msg) < findTimeNode tree  = (Node (insert msg tree) node more )
	| findTimeMsg msg > findTimeNode tree    = (Node less node (insert msg tree))
	| findTimeMsg msg == findTimeNode tree   = (Node less msg more)
	| otherwise                              = tree
		where 
			less = dupTreeLess tree 
			node = dupTreeNode tree 
			more = dupTreeMore tree 

-- Step 1:  Isolate the TimeStamp variable from a LogMessage input
findTimeMsg :: LogMessage -> TimeStamp
findTimeMsg (LogMessage _ time _ ) = time 

-- Step 2:  Isolate timeTampe of Node on Binary tree
findTimeNode :: MessageTree -> TimeStamp
findTimeNode (Node less node more) = findTimeMsg node 

-- Step 3:   Isolate "less tree"
dupTreeLess :: MessageTree -> MessageTree
dupTreeLess (Node less _ _)    = less
dupTreeLess (less)             = less 

-- Step 4:  Isolate "node"
dupTreeNode :: MessageTree -> LogMessage
dupTreeNode (Node _ node _)    = node
 
-- Step 5:  Isolate "right tree"
dupTreeMore :: MessageTree -> MessageTree
dupTreeMore (Node _ _ more )     = more 
dupTreeMore (more)               = more 

