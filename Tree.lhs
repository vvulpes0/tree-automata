> module Tree where

> -- |Nonempty trees of arbitrary width
> data Tree a = Tree { rootLabel :: a
>                    , children  :: [Tree a]
>                    } deriving (Eq, Ord, Read, Show)
