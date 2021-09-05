> module Acceptor where

> import Data.Foldable (toList)
> import Data.List (nub, partition)
> import Data.Map.Strict (Map)
> import Data.Set (Set)
> import qualified Data.Map.Strict as Map
> import qualified Data.Set as Set

> import Tree


Types
=====

> -- |An identity container.
> data I x = I x deriving (Eq, Ord, Read, Show)
> instance Foldable I where
>     foldr f a (I x) = f x a

> -- |A homogeneous function from @(x n)@ to @(x n)@.
> -- This is a deterministic unlabelled graph,
> -- (strongly directed) hypergraph, etc.
> data HomoFunc x n
>     = HomoFunc
>     { elements    :: Set n
>     , connections :: Map (x n) (x n)
>     }
>     deriving (Eq, Ord, Read, Show)

> -- |Transform node labels.
> renameStatesF :: (Ord b, Ord (x b), Functor x) =>
>                  (n -> b) -> HomoFunc x n -> HomoFunc x b
> renameStatesF f h
>     = HomoFunc
>       { elements    = Set.map f (elements h)
>       , connections = Map.mapKeys (fmap f)
>                       . Map.map (fmap f) $ connections h
>       }

> -- |A labelled graph as a set of overlaid graphs.
> data LGraph e x n
>     = LGraph
>       { overlays :: Map e (HomoFunc x n) }
>       deriving (Eq, Ord, Read, Show)

> -- |Transform edge labels.
> renameEdgesL :: Ord b => (e -> b) -> LGraph e x n -> LGraph b x n
> renameEdgesL f = LGraph . Map.mapKeys f . overlays

> -- |A DFA. Strings can be represented by @(Acceptor Maybe)@,
> -- while trees use @(Acceptor [])@.
> -- For strings, the initial state is @Nothing@.
> -- For trees, there is no intial state, but leaves have no sources.
> data Acceptor e x n
>     = Acceptor
>       { alphabet    :: Set e
>       , transitions :: LGraph e x n
>       , finals      :: Set n
>       }
>     deriving (Eq, Ord, Read, Show)

> -- |Transform state labels.
> renameStates :: (Ord b, Ord (x b), Functor x) =>
>                 (n -> b) -> Acceptor e x n -> Acceptor e x b
> renameStates f a
>     = Acceptor
>       { alphabet = alphabet a
>       , transitions = LGraph . Map.map (renameStatesF f)
>                       . overlays $ transitions a
>       , finals = Set.map f $ finals a
>       }

> -- |Transform edge labels.
> renameEdges :: Ord b => (e -> b) -> Acceptor e x n -> Acceptor b x n
> renameEdges f a = a { alphabet = Set.map f $ alphabet a
>                     , transitions = renameEdgesL f $ transitions a
>                     }

> -- |All states represented by an acceptor.
> states :: (Foldable x, Ord n) => Acceptor e x n -> Set n
> states a = Set.union edgeEnds $ finals a
>     where statesG = concatMap combine . Map.assocs . connections
>           combine (a, b) = toList a ++ toList b
>           edgeEnds = Set.fromList
>                      . concatMap statesG
>                      . Map.elems
>                      . overlays
>                      $ transitions a

> -- |A homogeneous relation from @(x n)@ to @(x n)@.
> -- This is a not-necessarily-deterministic unlabelled graph,
> -- (strongly directed) hypergraph, etc.
> data HomoRel x n
>     = HomoRel
>     { elementsR    :: Set n
>     , connectionsR :: Map (x n) (Set (x n))
>     }
>     deriving (Eq, Ord, Read, Show)

> type Graph n = HomoRel I n


Construction
============

> -- |Turn a context-free grammar into a tree acceptor
> -- by giving each symbol its own state.
> -- Terminals (leaves) should be marked as having no sources.
> -- The container type should be @[]@ for an actual CFG,
> -- but this is written in a more general way.
> fromCFG :: (Ord n, Foldable x, Applicative x, Ord (x n)) =>
>            [(n, x n)] -> Set n -> Acceptor n x n
> fromCFG xs fs = fromtrs (map (\(a,b) -> (a,b,a)) xs) fs

> -- |Specify an acceptor by its transitions and final states.
> fromtrs :: (Ord e, Ord n, Foldable x, Applicative x, Ord (x n)) =>
>            [(e, x n, n)] -> Set n -> Acceptor e x n
> fromtrs xs fs
>     = Acceptor
>       { alphabet = Set.fromList $ map (\(a,_,_) -> a) xs
>       , transitions = LGraph
>                       . foldr (Map.unionWith c) Map.empty
>                       $ map f xs
>       , finals = fs
>       }
>     where sts = Set.fromList
>                 $ concatMap (\(_,i,o) -> o : toList i) xs
>           f (e, i, o) = Map.singleton e . HomoFunc sts
>                         $ Map.singleton i (pure o)
>           c a b = HomoFunc sts $ Map.union (connections a) (connections b)


Computation
===========

> -- |True iff the given tree is accepted by the given acceptor.
> acceptT :: (Ord e, Ord n) => Acceptor e [] n -> Tree e -> Bool
> acceptT g = maybe False (flip elem (finals g)) . stateT g

> -- |The state assigned to the root node of the given tree.
> stateT :: (Ord e, Ord n) => Acceptor e [] n -> Tree e -> Maybe n
> stateT a t = join
>              . join
>              . fmap (fmap safeHead)
>              . fmap (flip Map.lookup (connections g))
>              . sequence . map (stateT a)
>              $ children t
>     where emptyG = HomoFunc Set.empty Map.empty
>           g = Map.findWithDefault emptyG (rootLabel t)
>               . overlays $ transitions a


Reachability and satisfiability
===============================

> -- |States reachable from a set of states
> reachables :: (Foldable x, Ord n) =>
>               LGraph e x n -> Set n -> Set n
> reachables g s
>     | t == s    = s
>     | otherwise = reachables g t
>     where r = collectStates
>               . Map.elems
>               . Map.filterWithKey (\k _ -> all (flip elem s) k)
>               . connections
>           t = foldr Set.union s . map r . Map.elems $ overlays g

> -- |Remove inaccessible states and their assoicated (hyper)edges.
> reduce :: (Foldable x, Ord n) => Acceptor e x n -> Acceptor e x n
> reduce g = Acceptor
>            { alphabet = alphabet g
>            , transitions
>            = LGraph
>              { overlays = Map.map h (overlays (transitions g)) }
>            , finals = Set.intersection s $ finals g
>            }
>     where s = reachables (transitions g) Set.empty
>           f = Map.filterWithKey (\k a -> all (flip elem s) k
>                                          && all (flip elem s) a)
>           h x = HomoFunc (elements x) (f $ connections x)

> -- |True iff no structure is accepted by the acceptor.
> isEmpty :: (Foldable x, Ord n) => Acceptor e x n -> Bool
> isEmpty = null . finals . reduce


Minimization and normal forms
=============================

> -- |Canonical form is trimmed, reduced, and minimized.
> canonicalize :: (Ord n, Foldable x, Functor x, Ord (x n)) =>
>                 (Acceptor e x n -> [Set n])
>              -> Acceptor e x n -> Acceptor e x n
> canonicalize cgen = trim . minimize cgen . reduce

> -- |Coalesce equivalent states.
> minimize :: (Ord n, Functor x, Ord (x n)) =>
>             (Acceptor e x n -> [Set n])
>          -> Acceptor e x n -> Acceptor e x n
> minimize cgen a = renameStates f a
>     where f n = maybe n id . safeHead . Set.toList
>                 . Set.unions . filter (elem n) $ cgen a

> -- |Remove states with no path to a final state.
> -- Note: the input must be minimal.
> trim :: (Foldable x, Ord n, Ord (x n)) => Acceptor e x n -> Acceptor e x n
> trim a
>     = a { transitions = LGraph . Map.map rm . overlays $ transitions a }
>     where qs = states a `Set.difference` finals a
>           es = Map.unionsWith Set.union
>                . map (Map.map Set.singleton . connections)
>                . Map.elems . overlays $ transitions a
>           -- @f q@: true iff all edges from q lead back to q
>           f q = Map.null
>                 (Map.filterWithKey
>                  (\k a -> elem q k && any (not . elem q) a)
>                  es)
>           r  = Set.filter f qs -- useless states
>           rm h = HomoFunc
>                  { elements = elements h `Set.difference` r
>                  , connections
>                    = Map.filterWithKey
>                      (\k a -> all (flip notElem r) k
>                               && all (flip notElem r) a)
>                      $ connections h
>                  }

> -- |Given a list of sets, return a list of disjoint sets
> -- grouping all pairs that were originally grouped.
> classes :: Ord n => [Set n] -> [Set n]
> classes (x:xs) = Set.unions p : classes rest
>     where (p, q) = partition (not . Set.disjoint x) (x:xs)
>           c = Set.unions p
>           rest = filter (Set.disjoint c) q
> classes _ = []

> -- |Nonsingleton equivalence classes under Nerode's relation.
> nerodeT :: (Ord e, Ord n) => Acceptor e [] n -> [Set n]
> nerodeT a = classes
>             . filter (not . null)
>             . (map (maybe Set.empty Set.fromList
>                    . sequence . Set.toList . Set.delete Nothing))
>             $ Set.toList indistincts
>     where qs = Set.insert Nothing . Set.map Just $ states a
>           pairs = nub
>                   . (concatMap
>                      (\a -> map (flip Set.insert a) (Set.toList qs)))
>                   . map Set.singleton
>                   $ Set.toList qs
>           f  = Set.map Just $ finals a
>           nf = Set.fromList
>                $ (filter
>                   (\p -> any (flip elem f) p
>                          && any (not . flip elem f) p)
>                   pairs)
>           os = overlays $ transitions a
>           sources = map (map Just)
>                     . concatMap (Map.keys . connections)
>                     $ Map.elems os
>           go x s = safeHead
>                    . (maybe []
>                       (flip (Map.findWithDefault [])
>                       . Map.findWithDefault Map.empty x
>                       $ Map.map connections os))
>                    $ sequence s
>           dist p q
>               = case Set.toList q of
>                   (m:n:_)
>                       -> let ss = concatMap (swapOne m n) sources
>                          in (any (not . flip elem p)
>                             $ (concatMap
>                                (\x ->
>                                 (map
>                                  (\(a,b) -> Set.fromList [go x a, go x b])
>                                  ss))
>                                (Set.toList $ alphabet a)))
>                   _ -> False
>           (indistincts,_)
>               = (until
>                  (uncurry (==))
>                  (\(_,c) ->
>                   (c, Set.filter (not . dist c) c)
>                  )
>                  (Set.empty, Set.difference (Set.fromList pairs) nf))


Finiteness
==========

To check for finiteness, build a connection graph and check for cycles.

> -- |For graphlike objects that allow more or fewer than one node
> -- as sources or sinks of connections, construct a standard graph
> -- that relates every source-sink pair of each edge.
> connectionGraph :: (Ord n, Foldable x) => Acceptor e x n -> Graph n
> connectionGraph = foldr overlay (HomoRel Set.empty Map.empty)
>                   . map splitG . Map.elems . overlays . transitions

> -- |For graphlike objects that allow more or fewer than one node
> -- as sources or sinks of connections, construct a relation where
> -- each source relates to each sink.
> splitG :: (Ord n, Foldable x) => HomoFunc x n -> Graph n
> splitG g = HomoRel
>            { elementsR = elements g
>            , connectionsR = Map.fromListWith Set.union
>                             . concatMap (uncurry cs) . Map.assocs
>                             $ connections g
>            }
>     where cs xs = Set.toList . Set.cartesianProduct (toSet xs)
>                   . Set.mapMonotonic Set.singleton . toSet
>           toSet = foldr (\a b -> Set.insert (I a) b) Set.empty

> -- |The union of two relations.
> overlay :: (Ord n, Ord (x n)) =>
>            HomoRel x n -> HomoRel x n -> HomoRel x n
> overlay a b = HomoRel
>               { elementsR = elementsR a `Set.union` elementsR b
>               , connectionsR = Map.unionWith Set.union
>                                (connectionsR a)
>                                (connectionsR b)
>               }

> -- |The transitive closure of a graph.
> tClose :: Ord n => Graph n -> Graph n
> tClose g = fst . until (uncurry (==)) (\(_,b) -> (b, step b))
>            $ (HomoRel Set.empty Map.empty, g)
>     where step x = HomoRel
>                    { elementsR = elementsR g
>                    , connectionsR = Map.map (f (connectionsR x))
>                                     $ connectionsR x
>                    }
>           f m qs = Set.unions
>                    . map (flip (Map.findWithDefault Set.empty) m)
>                    $ Set.toList qs

> -- |True iff any node can reach itself via one or more nonempty paths.
> isCyclic :: Ord n => Graph n -> Bool
> isCyclic = any (uncurry elem) . Map.assocs . connectionsR . tClose

> -- |True iff only finitely many structures satisfy the acceptor.
> isFinite :: (Ord e, Ord n) => Acceptor e [] n -> Bool
> isFinite = not . isCyclic . connectionGraph . canonicalize nerodeT


Utility functions
=================

> collectStates :: (Foldable x, Ord n) => [x n] -> Set n
> collectStates = Set.fromList . concatMap toList

> safeHead :: [a] -> Maybe a
> safeHead xs = case xs of
>                 (a:_) -> Just a
>                 _     -> Nothing

> join :: Monad m => m (m a) -> m a
> join = flip (>>=) id

> replaceOne :: Eq a => a -> a -> [a] -> [[a]]
> replaceOne _ _ [] = []
> replaceOne a b (x:xs)
>     | x == a = (b : xs) : rest
>     | otherwise = rest
>     where rest = map (x:) $ replaceOne a b xs

> swapOne :: Eq a => a -> a -> [a] -> [([a],[a])]
> swapOne a b xs = zip (repeat xs) (replaceOne a b xs ++ replaceOne b a xs)
