-- (2 балла)

import Control.Applicative

data Tree a = Node a [Tree a]

instance Functor Tree where
    fmap f (Node v c) = Node (f v) $ map (fmap f) c

instance Applicative Tree where
    pure a = t' where t' = Node a repeat t'
    (<*>) = undefined
