-- UPENN HASKELL HOMEWORK - #2
{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- Exercise 1:   The first step is figuring out how to parse an individual
-- message. Define a function
-- parseMessage :: String -> LogMessage
-- which parses an individual line from the log file.

test_input = [LogMessage Info 5053 "pci_id: con ing!",LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)",LogMessage Warning 3654 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled",LogMessage Info 4076 "verse.'",LogMessage Info 4764 "He trusts to you to set them free,",LogMessage Info 858 "your pocket?' he went on, turning to Alice.",LogMessage Info 898 "would be offended again.",LogMessage Info 3753 "pci 0x18fff steresocared, overne: 0000 (le wailan0: ressio0/derveld fory: alinpu-all)",LogMessage Info 790 "those long words, and, what's more, I don't believe you do either!' And",LogMessage Info 3899 "hastily.",LogMessage Info 2194 "little creature, and held out its arms and legs in all directions, 'just",LogMessage Info 1447 "she was terribly frightened all the time at the thought that it might be",LogMessage Info 1147 "began ordering people about like that!'",LogMessage Info 3466 "pci_hcd beed VRAM=2)",LogMessage Info 3974 "#55500:00000 (nux Us nel chablesen ster C)",LogMessage Info 3724 "Laughing and Grief, they used to say.'",LogMessage Info 1283 "'Now tell me, Pat, what's that in the window?'",LogMessage Info 4469 "'If that's all you know about it, you may stand down,' continued the",LogMessage Info 1641 "'I feared it might injure the brain;",LogMessage Info 1744 "aloud; and in another moment it was out of sight.",LogMessage (Error 47) 1034 "'What a pity it wouldn't stay!' sighed the Lory, as soon as it was quite",LogMessage Info 3284 "uhcd 00:03.3: LNVS) @ 0000000:0174715:00000000000] BOOTMEM",LogMessage Info 4018 "VGA mem Dynabled, nor memodiregis nosaved)",LogMessage Info 3304 "DMA32: 5",LogMessage Info 2920 "all of them bowed low.",LogMessage Warning 1654 "'I kept all my limbs very supple",LogMessage Info 2803 "'They were learning to draw,' the Dormouse went on, yawning and rubbing",LogMessage Info 2788 "from?'",LogMessage Info 2378 "vanishing so suddenly: you make one quite giddy.'",LogMessage Info 2669 "pci_hci 000: 000:1c.1: RCONF(NETDEV_UP): 6232 has numalithis pregista_sone bus 00000-0x000009ff]",LogMessage Info 861 "#66WW-1a.2:e680fed13 00:1a00: adevicesetlindow [0xa0] (idged",LogMessage Info 4751 "pci 0001: [mem 000.58 (to 6-0xf43devialligurcpice wing ling IOM by 32 kipts ask: fa0000:1c.1: T4062",LogMessage Info 4391 "'Give your evidence,' the King repeated angrily, 'or I'll have you",LogMessage Info 1883 "cpi 0xf42 by EHCI ting CRT]",LogMessage Info 4436 "raw0::throl: atardb0fead: brit 10:24: Slock",LogMessage Info 2382 "Seto 00: Regispor suppow) st sp stus cold a00009dc0000003.2:e6:02. TCONF(NETDEV_UP): e3/0xa000000fed",LogMessage Info 2751 "Allencortenablerveres irq 9 pi 0000000000:1d.7: se fory 64",LogMessage Info 2965 "6 fortc009d908:c000000000",LogMessage Info 1108 "had vanished completely.",LogMessage Info 1862 "'Well! WHAT are you?' said the Pigeon. 'I can see you're trying to",LogMessage Info 4980 "would, in the after-time, be herself a grown woman; and how she would",LogMessage Info 3260 "pci_hci 00:0: Rounter to con upitchash 000 probe mem 0, PRT]",LogMessage Info 3062 "Brot D0 -> Gracputer 1)",LogMessage Info 3720 "ACPIC -> Inith to lassound [LNXVID: PCI E-ID - 000000: EC: D3hosave winged 000000000: Ressio+mem 0000000971c.0:1fff000d7c2: 000:00e winge memodevicesentel: rom caterve ouppoot-cardev/deve mort D0 Duot PM: Usign=3)",LogMessage Info 2521 "ACPI: wlap://indow [iore:1d.",LogMessage (Error 3) 162 "long passage, and the White Rabbit was still in sight, hurrying down it.",LogMessage Info 2218 "yourself.'",LogMessage Warning 2288 "fb 15 bus=io 0000000000001: atard000000 MTRRO/100: Ref CPU0:000000 - 001: modifieseterventint-ink at updarders /deon: entive hidge resettink irq 11ZCT0, lize: mem 000:0:15:0000: Registespory ting ed1c.4:6c:c200 dirq 4 (ors 00000:000: BASPM: PMDISK: padeved)",LogMessage Info 4781 "'That's the most important piece of evidence we've heard yet,' said the",LogMessage Warning 1958 "The Fish-Footman began by producing from under his arm a great letter,"]


-- Test data, one of each type:  Info, Error, Warning. 
info' = "I 4764 He trusts to you to set them free"
error' = "E 47 1034 'What a pity it wouldn't stay!' sighed the Lory, as soon as it was quite"
warning' = "W 1654 'I kept all my limbs very supple"
-- end test data


parse :: String -> [LogMessage]
parse = map parseMessage . lines
 

parseMessage :: String -> LogMessage
parseMessage (x:xs)
	| x == 'I'   = LogMessage msgType msgTime msgString 
	| x == 'E'   = LogMessage msgType msgTime msgString 
	| x == 'W'   = LogMessage msgType msgTime msgString 
	| otherwise  = Unknown (x:xs)
	where
		msgType = getMsgType $ breakUp (x:xs)
		msgTime = getTimestamp $ breakUp (x:xs)
		msgString = getTrailingString $ breakUp (x:xs)
-- Can get TimeStamp with:  getTimestamp $ breakUp error'



-- Take raw string, and list full type name + error severity
configType :: String -> String
configType (x:xs)
	| x == 'I'   =  "Info" ++ xs
	| x == 'E'   =  "(Error" ++ errNum ++ " ) " ++ errStr
	| x == 'W'   =  "Warning" ++ xs
	| otherwise  =  "error at configType"
		where 
			errNum = take 5 xs
			errStr = drop 3 xs  

-- break up configured type into list of words
breakUp :: String -> [String]
breakUp input = words (configType input)


-- Take breakUp output and get the Timestamp
getTimestamp :: [String] -> TimeStamp 
getTimestamp list
	| list !! 0 == "Info"     = read $ list !! 1 :: TimeStamp
	| list !! 0 == "(Error"   = read $ list !! 4 :: TimeStamp
	| list !! 0 == "Warning"  = read $ list !! 1 :: TimeStamp
	| otherwise               =	0


-- Use code above, repurpose
getMsgType :: [String] -> MessageType
getMsgType list
	| list !! 0 == "Info"     = Info :: MessageType
	| list !! 0 == "(Error"   = (Error (read $ list !! 1)) :: MessageType
	| list !! 0 == "Warning"  = Warning :: MessageType


-- Use code above, repurpose
getTrailingString :: [String] -> String 
getTrailingString list
	| list !! 0 == "Info"     = unwords $ drop 2 list :: String
	| list !! 0 == "(Error"   = unwords $ drop 5 list :: String
	| list !! 0 == "Warning"  = unwords $ drop 2 list :: String
	| otherwise               = "Error at getTrailingString"



-- Exercise 2 -- Insert function + Sorting messages

{- Test Data
   This is a Binary Search Tree built from test data and functions created in EX 1.
   To create:  Applied @parseMessage@ to the original data types, resulting in LogMessage types.  To test validity, use ":type testTree" in GHCI REPL -}
testTree = Node (Node Leaf (parseMessage error') Leaf) (parseMessage warning') (Node Leaf (parseMessage info') Leaf)


{- This is a single LogMesssage, built using the parseMessage from the above. Servers as a test for parseMessage AND an example LogMessage to use while building the helper functions.-}
testMsg = parseMessage "I 1365 now--Don't choke him--How was it, old fellow? What happened to you? Tell"

-- End Test Data


insert :: LogMessage -> MessageTree -> MessageTree
insert msg Leaf = Node Leaf msg Leaf
insert (Unknown _) tree = tree 
insert _ tree@(Node _ (Unknown _) _) = tree 
insert msg tree 
	| findTimeMsg msg < findTimeNode tree  = (Node (insert msg less) node more )
	| findTimeMsg msg > findTimeNode tree  = (Node less node (insert msg more))
	| findTimeMsg msg == findTimeNode tree = (Node less msg more)
	| otherwise                            = (Node less msg more)
		where 
			less = dupTreeLess tree 
			node = dupTreeNode tree 
			more = dupTreeMore tree 

-- Step 1:  Isolate the TimeStamp variable from a LogMessage input
findTimeMsg :: LogMessage -> TimeStamp
findTimeMsg (LogMessage _ time _ ) = time 

-- Step 2:  Isolate TimeStamp of Node on Binary tree
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




-- Exercise 3:  Build a tree using all the LogMessagess. Start with a Leaf. 
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4:  Define a function that takes a sorted MessageTree and produces a list of all the LogMessages it contains, sorted by timestamp from smallest to biggest.  Also remove Unknown messages. and sort the well-formed messages using an expression such as:  inOrder (build tree) 

inOrder :: MessageTree -> [LogMessage]
inOrder tree = inOrder' tree []

inOrder' :: MessageTree -> [LogMessage] -> [LogMessage]
inOrder' Leaf list = list 
inOrder' (Node Leaf msg Leaf)  list  = msg : list 
inOrder' (Node left msg Leaf)  list  = inOrder' left (msg : list)
inOrder' (Node Leaf msg right) list  = msg : inOrder' right list  
inOrder' (Node left msg right) list  = inOrder' left (msg : inOrder' right list) 




-- Exercise 5:  Take an UN-ordered list, and extract only the ERRORS with a severity of 50+. Resulting list must be sorted by TimeStamp.

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = map getErrMsg (filter (getErrors) list)
	where
		list = inOrder (build messages)

getErrors :: LogMessage -> Bool
getErrors (LogMessage (Error y) _ _ ) = y > 50
getErrors _                           = False

getErrMsg :: LogMessage -> String
getErrMsg (LogMessage _ _ msg) = msg
getErrMsg _ = ""

-- NEXT STEPS:
-- Make this work:  Order by Timestamp via @insert@ into a Binary Tree, MessageTree