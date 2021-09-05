> module Main where

> import Acceptor
> import Visualizer

> import qualified Data.Set as Set

> main :: IO ()
> main = putStr . toDot $ s

> s :: Acceptor String [] String
> s = fromCFG
>     [ ("S",["a","S","b"])
>     , ("S",["a","b"])
>     , ("a",[])
>     , ("b",[])
>     ]
>     $ Set.singleton "S"

> abba :: Acceptor String [] String
> abba = fromtrs
>        [ ("a",[],"A")
>        , ("b",[],"B")
>        , ("S",["A","B"],"AB")
>        , ("S",["B","A"],"BA")
>        , ("S",["A","AB","B"],"AB")
>        , ("S",["A","BA","B"],"AB")
>        , ("S",["B","AB","A"],"BA")
>        , ("S",["B","BA","A"],"BA")
>        ]
>        $ Set.fromList ["AB","BA"]
