-- Define a tree type that has only one constructor, like our Java example. Instead of
-- the Empty constructor, use the Maybe type to refer to a node’s children.

data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a)) deriving Show

main = putStrLn $ show (Node 1 (Just (Node 2 Nothing Nothing)) (Just (Node 3 Nothing Nothing)))
