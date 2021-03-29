> module Main where

> import TreeAcceptor
> import Visualizer

> import qualified Data.Set as Set

> main :: IO ()
> main = putStr . toDot $ s

> s :: Acceptor String [] String
> s = end (Set.singleton "S")
>     . add "a" [] ["A"]
>     . add "b" [] ["B"]
>     . add "S" ["A","B"] ["S"]
>     . add "S" ["A","S","B"] ["S"]
>     $ empty
