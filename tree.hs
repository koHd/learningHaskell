data Tree = Leaf Int | Node Op Tree Tree
    deriving Show

data Op = Add | Mult
    deriving Show

reduce (Leaf n) = n
reduce (Node Add left right) = (reduce left) + (reduce right)
reduce (Node Mult left right) = (reduce left) * (reduce right)
