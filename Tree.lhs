> module Tree where

> -- |Nonempty trees of arbitrary width.
> data Tree a = Tree a [Tree a] deriving (Eq, Ord, Read, Show)
