> module Visualizer where

> import Data.List (intercalate)
> import Data.Set  (Set)
> import qualified Data.Set as Set
> import qualified Data.Map.Strict as Map

> import Acceptor

> toDot :: Acceptor String [] String -> String
> toDot a = unlines
>           [ "digraph {"
>           , "  graph[rankdir=\"LR\"];"
>           , "  node [fixedsize=\"false\","
>           , "        style=\"filled\","
>           , "        color=\"black\","
>           , "        fillcolor=\"white\","
>           , "        fontcolor=\"black\","
>           , "        fontsize=\"12.0\","
>           , "        width=\"0.5\","
>           , "        height=\"0.5\"];"
>           , "  edge [fontsize=\"12.0\","
>           , "        arrowsize=\"0.5\"];"
>           ]
>           ++ unlines (map state $ Set.toList qs)
>           ++ unlines (concatMap f e)
>           ++ unlines ["}"]
>     where trs = map (fmap (Map.assocs . connections))
>                 . Map.assocs . overlays $ transitions a
>           e = zip [1..] $ concatMap (\(x,ps) -> map ((,) x) ps) trs
>           qs = states a
>           f (n, (x, (i, o))) = [sym n x]
>                                ++ (map (\(j,m) -> conn m j (show n))
>                                    (zip (map show [0..]) i))
>                                ++ map (\q -> conn (show n) "" q) o
>           state q = "  " ++ q ++ ";"
>           sym n x = "  " ++ show n ++ " [label=\"" ++ x ++ "\""
>                     ++ ",fillcolor=\"black\",fontcolor=\"white\""
>                     ++ ",width=\"0.25\",height=\"0.25\"];"
>           conn q x r = "  " ++ q ++ " -> " ++ r
>                        ++ " [label=\"" ++ x ++ "\"];"
