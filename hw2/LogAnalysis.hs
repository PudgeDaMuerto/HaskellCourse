module LogAnalysis () where
 
import Log

parseMessage :: String -> LogMessage
parseMessage xs = case words xs of
    ("I" : time : message)           -> createLog Info time message
    ("W" : time : message)           -> createLog Warning time message
    ("E" : errCode : time : message) -> createLog (Error $ read errCode) time message
    _                                -> Unknown xs
    where
        createLog :: MessageType -> String -> [String] -> LogMessage
        createLog m t ms = LogMessage m (read t) (unwords ms)

parse :: String -> [LogMessage]
parse xs = map parseMessage $ lines xs

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert newLog Leaf = Node Leaf newLog Leaf
insert newLog@(LogMessage _ newLogTime _) (Node left nodeLog@(LogMessage _ nodeLogTime _) right)
    | newLogTime < nodeLogTime = Node (insert newLog left) nodeLog right
    | otherwise = Node left nodeLog (insert newLog right)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left log right) = inOrder left ++ [log] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = [errText | (LogMessage (Error lvl) _ errText) <- xs, lvl > 50]
    where
        xs = inOrder . build $ logs