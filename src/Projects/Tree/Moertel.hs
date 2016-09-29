module Moertel where

data Tree a = Leaf a | Node a (Tree a) (Tree a)
    deriving (Eq, Show)

preorder f z tree = go tree z
  where
    go (Leaf v)     z = f v z
    go (Node v l r) z = let z'   = f v z     -- current node
                            z''  = go l z'   -- left subtree
                            z''' = go r z''  -- right subtree
                        in z'''


inorder f z tree = go tree z
  where
    go (Leaf v)     z = f v z
    go (Node v l r) z = let z'   = go l z    -- left subtree
                            z''  = f v z'    -- current node
                            z''' = go r z''  -- right subtree
                        in z'''


postorder f z tree = go tree z
  where
    go (Leaf v)     z = f v z
    go (Node v l r) z = let z'   = go l z    -- left subtree
                            z''  = go r z'   -- right subtree
                            z''' = f v z''   -- current node
                        in z'''




tree7 = Node 7
            (Node 1
                (Leaf 0)
                (Node 3
                    (Leaf 2)
                    (Node 5 (Leaf 4) (Leaf 6))))
            (Node 9
                (Leaf 8) (Leaf 10))


tree4 = Node 4
            (Node 2 (Leaf 1) (Leaf 3))
            (Node 5 (Leaf 6) (Leaf 7))

tree15 = Node 8
            (Node 4
                (Node 2 (Leaf 1) (Leaf 3))
                (Node 5 (Leaf 6) (Leaf 7)))
            (Node 12
                (Node 10 (Leaf 9) (Leaf 11))
                (Node 14 (Leaf 13) (Leaf 15)))

--- NOTE
-- reversing the result is normal as it is ok that they print out backwards.
-- see: http://blog.moertel.com/posts/2012-01-26-the-inner-beauty-of-tree-traversals.html

-- 7, 1, 0, 3, 2, 5, 4, 6, 9, 8, 10
pre = reverse $ preorder (:) [] tree7
-- 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
ino = reverse $ inorder (:) [] tree7
-- 0, 2, 4, 6, 5, 3, 1, 8, 10, 9, 7
post = reverse $ postorder (:) [] tree7