> module TreeAcceptor
>     ( -- * Types
>       Acceptor(..)
>     , LGraph(overlays)
>     , HomoRel(elements, connections)
>     , I(..)
>       -- * Creation
>     , add
>     , end
>     , empty
>       -- * Information
>     , accepts
>     , states
>       -- * Transformations
>     , reversal
>     , unlabel
>     , unorder
>       -- * Decision problems
>     , isEmpty
>     ) where

> import Tree

> import Data.List (intercalate)
> import Data.Set (Set)
> import Data.Map (Map)

> import qualified Data.Set as Set
> import qualified Data.Map as Map

> add :: (Ord n, Ord (x n), Settable x, Ord e) =>
>        e -> x n -> x n -> Acceptor e x n -> Acceptor e x n
> add lbl source sink g
>     = g {transitions = relateBy (Just lbl) source sink (transitions g)}

> end :: Set n -> Acceptor e x n -> Acceptor e x n
> end xs g = g { finals = xs }

> -- |True iff the bottom-up tree acceptor recognizes the given tree.
> accepts :: (Ord e, Ord n) => Acceptor e [] n -> Tree e -> Bool
> accepts g = not . Set.disjoint (finals g) . delta g

> -- |The state reached by the given (sub)tree.
> delta :: (Ord e, Ord n) => Acceptor e [] n -> Tree e -> Set n
> delta g (Tree a xs)
>     = Set.fold f Set.empty . Set.unions
>       $ map (flip (Map.findWithDefault Set.empty) r) seqs
>     where r = connections . Map.findWithDefault emptyRel (Just a)
>               . overlays $ transitions g
>           seqs = spellOut $ map (delta g) xs
>           f xs a = case xs of
>                      (y:ys) -> Set.insert y a
>                      _      -> a

> -- |True iff no tree will ever satisfy the needs of the acceptor.
> isEmpty :: (Ord e, Ord n) => Acceptor e [] n -> Bool
> isEmpty g = Set.disjoint (finals g)
>             . flip satisfiables Set.empty
>             . unlabelL . transitions
>             . reversal $ unorder g

The @satisfiables@ function wants
a reversed unlabeled directed hypergraph.
In a DFA powerset construction, the set implies a choice of states.
Here the set is not a nondeterministic "ANY" but an "ALL".
So, a state is satisfiable if there is a transition for which
every destination state is also satisfiable;
each child can actually have a subtree built from it.

> -- |When called with an empty initial frontier,
> -- yields all and only those states that the acceptor can accept from.
> satisfiables :: (Ord n) => HomoRel Set n -> Set n -> Set n
> satisfiables r frontier
>     | Set.null nexts = frontier
>     | otherwise = satisfiables r (Set.union nexts frontier)
>     where as = Set.difference (elements r) frontier
>           nexts = Set.unions . Map.keys . Map.filterWithKey f
>                   $ connections r
>           f k a = not (Set.disjoint k as) -- k is singleton. new?
>                   && any (flip Set.isSubsetOf frontier) (Set.toList a)

> -- |Turn all edges into epsilon edges.
> unlabel :: (Ord n, Ord (x n)) => Acceptor e x n -> Acceptor () x n
> unlabel g = g {transitions = LGraph
>                              . flip (Map.insert Nothing) Map.empty
>                              . unlabelL $ transitions g
>               }

> -- |Overlay all the overlays, forget the labels exist.
> unlabelL :: (Ord n, Ord (x n)) => LGraph e x n -> HomoRel x n
> unlabelL = foldr combineRels emptyRel . Map.elems . overlays



> -- |All of the states that are present in the acceptor.
> states :: Ord n => Acceptor e x n -> Set n
> states = Set.unions . map elements . Map.elems . overlays . transitions

> -- |The graph where its edges have been inverted.
> reversal :: (Ord e, Ord n, Ord (x n)) =>
>             Acceptor e x n -> Acceptor e x n
> reversal g = g {transitions = ts'}
>     where ts' = LGraph (Map.map invert . overlays $ transitions g)

> -- |A new relation \(\mathrel{R^\prime}\) built from \(\mathrel{R}\)
> -- where \(x \mathrel{R^\prime} y\)
> -- iff \(y \mathrel{R} x\).
> invert :: (Ord n, Ord (x n)) =>
>           HomoRel x n -> HomoRel x n
> invert h = h { connections = Map.fromList inverted }
>     where kvs = Map.assocs $ connections h
>           inverted = map (fmap pres . join (,))
>                      . Set.toList . Set.unions $ map snd kvs
>           pres x = Set.fromList . map fst
>                    $ filter (Set.member x . snd) kvs

> -- |A graph with no content.
> empty :: Acceptor e x n
> empty = FSA (LGraph Map.empty) Set.empty Set.empty
> -- |A relation with no content.
> emptyRel :: HomoRel x n
> emptyRel = HomoRel Set.empty Map.empty

> -- |@relateBy x p q@ adds an edge labeled x from p to q.
> relateBy :: (Ord n, Ord (x n), Settable x, Ord e) =>
>             Maybe e -> x n -> x n -> LGraph e x n -> LGraph e x n
> relateBy lbl source sink g
>     = g {overlays = Map.insertWith combineRels
>                     lbl (relate source sink emptyRel)
>                     (overlays g)}

> -- |Overlay two relations. @x@ relates to @y@ iff the same holds
> -- in either source.
> combineRels :: (Ord n, Ord (x n)) =>
>                HomoRel x n -> HomoRel x n -> HomoRel x n
> combineRels r1 r2
>     = HomoRel { elements = Set.union (elements r1) (elements r2)
>               , connections = Map.unionWith Set.union
>                               (connections r1) (connections r2)
>               }

> -- |@relate source sink@ adds a connection from @source@ to @sink@.
> relate :: (Ord n, Ord (x n), Settable x) =>
>           x n -> x n -> HomoRel x n -> HomoRel x n
> relate source sink h
>     = h { elements = Set.unions [elements h
>                                 , toSet source
>                                 , toSet sink
>                                 ]
>         , connections = Map.insertWith Set.union
>                         source (Set.singleton sink)
>                         (connections h)
>         }


Types
=====

An acceptor is a graph that has (maybe) some initial states,
and some final states that have been marked.
For a bottom-up tree acceptor, the initial states
are implicit from the transitions, and so are ignored.

> data Acceptor e x n
>     = FSA
>       { transitions :: LGraph e x n
>       , initials :: Set n
>       , finals :: Set n
>       }
>     deriving (Eq, Ord, Read, Show)

The transitions themselves form a labeled (directed) graph.
One way to consider a labeled graph is as
a collection of <label, unlabeled graph> pairs.
Each unlabeled graph is the restriction to only the edges
implied by the corresponding label.

> data LGraph e x n
>     = LGraph
>       { overlays :: Map (Maybe e) (HomoRel x n) }
>     deriving (Eq, Ord, Read, Show)

A graph consists of some nodes and some connections between them.
Binary homogeneous relations form a directed graph,
where the nodes are the elements
and an edge appears from x to y iff x relates to y.

Sometimes the elements may be structured in some way.
For instance, if the relation connects sets to sets,
a directed hypergraph can be used
rather than including every distinct set as its own state.
The transitions of a tree-acceptor relate sequences of states to states,
or, being more general, sequences to sequences.
This can be represented by something akin to a hypergraph,
which I call a "strongly directed hypergraph",
where still only nodes themselves are stored
rather than introducing a new state for each possible sequence.

> data HomoRel x n
>     = HomoRel
>       { elements :: Set n
>       , connections :: Map (x n) (Set (x n))
>       }
>     deriving (Eq, Ord, Read, Show)

A strongly directed hypergraph encodes a lot of information.
Removing the ordering from the relation results in a standard hypergraph.
The @unorder@ function allows for this transformation.

> class Ordered c where
>     unorder :: Ord n => c [] n -> c Set n

> instance Ordered HomoRel where
>     unorder h = h {connections = Map.mapKeysWith Set.union toSet
>                                  . Map.map (Set.map toSet)
>                                  $ connections h}
>         where f (a, b) = (Set.fromList a, Set.fromList b)

> instance Ordered (LGraph e) where
>     unorder g = g {overlays = Map.map unorder (overlays g)}

> instance Ordered (Acceptor e) where
>     unorder g = g {transitions = unorder (transitions g)}

A collection is Settable if it can be converted to a @Set@.

> class Settable x where
>     toSet :: Ord a => x a -> Set a

> instance Settable Set where
>     toSet = id

> instance Settable [] where
>     toSet = Set.fromList

Because relations are defined here in a general way
that accounts for structure within the connected elements,
a type that asserts a lack of structure is needed
to account for, say, a standard String acceptor
where this extra structure is not assumed to exist.
We'll call this the @I@ type (for Identity).

> data I a = I a deriving (Eq, Ord, Read, Show)
> instance Settable I where
>     toSet (I a) = Set.singleton a


Generic Utility Functions
=========================

> join :: Monad m => m (m b) -> m b
> join = (>>= id)

> spellOut :: Ord a => [Set a] -> [[a]]
> spellOut [] = [[]]
> spellOut (x:xs) = concatMap (\s -> map (s :) (spellOut xs))
>                   $ Set.toList x
