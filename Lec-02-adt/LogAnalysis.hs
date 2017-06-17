module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage msg = transformMsg . words $ msg


transformMsg :: [String] -> LogMessage
transformMsg ("I":ts:msg) = LogMessage Info (read ts :: Int) (unwords msg)
transformMsg ("E":lvl:ts:msg) = LogMessage (Error (read lvl :: Int)) (read ts :: Int) (unwords msg)
transformMsg ("W":ts:msg) = LogMessage Warning (read ts :: Int) (unwords msg)
transformMsg msg = Unknown (unwords msg)


parseMessage2 :: String -> LogMessage
parseMessage2 line = let wordList = words line in
                     case wordList of
                       ("I":ts:msg) -> LogMessage Info (read ts :: Int) (unwords msg)
                       ("E":lvl:ts:msg) -> LogMessage (Error (read lvl :: Int)) (read ts :: Int) (unwords msg)
                       ("W":ts:msg) -> LogMessage Warning (read ts :: Int) (unwords msg)
                       _ -> Unknown (unwords wordList)

parse :: String -> [LogMessage]
parse fileStr = map parseMessage (lines fileStr)


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs =  map getMessage $ filter isSevere (inOrder (build logs))

isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error lvl) _ _) = lvl >= 50
isSevere _ = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ msg) = msg
getMessage (Unknown msg) = msg

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown msg) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ ts _) (Node left log@(LogMessage _ rootts _ ) right)
 | ts > rootts     = Node left log (insert msg right)
 | otherwise       = Node (insert msg left) log right

build :: [LogMessage] -> MessageTree
build logs = insertInto logs Leaf

insertInto :: [LogMessage] -> MessageTree -> MessageTree
insertInto [] tree = tree
insertInto (x : xs) tree = insertInto xs (insert x tree)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left log right) = (inOrder left) ++ [log] ++ inOrder(right)
