data Tree a = Emp | Node a (Tree a) (Tree a) deriving (Show, Eq, Ord)
exampleTree = Node 1 (Node 2 (Node 4 (Node 100 Emp Emp) Emp) (Node 5 Emp Emp)) (Node 3 (Node 6 Emp Emp) (Node 7 Emp Emp))

-- nummer 1
treeDepth Emp = 0
treeDepth (Node _ l r) = 1 + max (treeDepth l) (treeDepth r)

-- nummer 2
inorder Emp = []
inorder (Node elem l r) = inorder l ++ [elem] ++ inorder r

-- nummer 3 (unvollständig)
bfs Emp = []
bfs (Node elem l r) = elem : bfs l ++ bfs r

-- nummer 4
-- treemap f Emp = []
-- treemap f (Node elem l r) = f elem : treemap f l ++ treemap f r
treemap f Emp = Emp
treemap f (Node elem l r) = Node (f elem) (treemap f l) (treemap f r)

-- nummer 5
data ListTree a = LNode a [ListTree a] deriving Show
exampleListTree = LNode 1 [LNode 2 [], LNode 3 []]

-- 5b
sumOfNodes (LNode a l) = a + sumOfChildNodes l
sumOfChildNodes children = sum [sumOfNodes child | child <- children]

-- 5c

listTreemap f (LNode a l) = LNode (f a) (treemapChildren f l)
treemapChildren f children = [listTreemap f child | child <- children]