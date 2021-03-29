> module Visualizer where

> import TreeAcceptor

> import Data.List (intercalate)
> import Data.Set (Set)

> import qualified Data.Map as Map
> import qualified Data.Set as Set

Visualization
=============

> class Listable c where
>     toList :: Ord a => c a -> [(String,a)]

> instance Listable [] where
>     toList = zip (map show [0..])
> instance Listable Set where
>     toList = map ((,) "") . Set.toList
> instance Listable I where
>     toList (I a) = [("",a)]

> class Dottable c where
>     toDot :: c -> String

> instance (Show e, Show n, Listable x, Ord e, Ord n, Ord (x n)) =>
>     Dottable (Acceptor e x n) where
>     toDot g = unlines $ "digraph {" : filter (not . null) go ++ ["}"]
>         where go = concat [doHeader, doStates, doTrans]
>               ss = states g
>               numbered = zip [1..] . Set.toList $ ss
>               indexes x = map fst . filter ((== x) . snd)
>               index x   = head . (++ [0]) $ indexes x numbered
>               doHeader = [ "graph [rankdir=\"LR\"];"
>                          , "node  [fixedsize=\"false\","
>                            ++ "fontsize=\"12.0\","
>                            ++ "height=\"0.5\","
>                            ++ "width=\"0.5\"];"
>                          , "edge  [fontsize=\"12.0\","
>                            ++ "arrowsize=\"0.5\"];"
>                          ]
>               doStates = map doState . Set.toList $ ss
>               doState x = show (index x)
>                           ++ " [label=\"" ++ dq (show x) ++ "\""
>                           ++ (if Set.member x (finals g)
>                              then ",peripheries=\"2\""
>                              else "")
>                           ++ "];"
>               doTrans = concat . doTrans'
>                         $ zip [1..] (uniqueTransitions g)
>               doTrans' [] = []
>               doTrans' ((i, (e, ins, out)):xs)
>                   = let tq = "T" ++ show i
>                     in ((tq ++ " [label=\""
>                          ++ maybe "" (dq . show) e
>                          ++ "\",style=\"filled\"];")
>                         : showSeq tq (toList ins)
>                         ++ showOut tq (toList out))
>                        : doTrans' xs
>               showSeq s ns = let qs = Set.toList . Set.fromList
>                                       $ map snd ns
>                                  unbr = filter (not . flip elem "[]")
>                                  f q = show (index q)
>                                        ++ " -> " ++ s
>                                        ++ " [label=\""
>                                        ++ unbr (intercalate ","
>                                                 (indexes q ns))
>                                        ++ "\"];"
>                              in map f qs
>               showOut s = map (\(_,x) -> s ++ " -> " ++ show (index x))

> uniqueTransitions :: (Ord e, Ord (x n)) =>
>                      Acceptor e x n -> [(Maybe e, x n, x n)]
> uniqueTransitions = concatMap f . Map.assocs . overlays . transitions
>     where f (e, r) = concatMap (g e) . Map.assocs $ connections r
>           g e (k, a) = Set.toList $ Set.map ((,,) e k) a

> dq :: String -> String
> dq = filter (/= '"')
