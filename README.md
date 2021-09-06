Tree Acceptors
==============

Introduction
------------

This is the beginning of a simple library
for bottom-up finite-state tree acceptors.


Currently implemented are:

* Constructing trees and acceptors
* A visualization of tree acceptors
* Reduction to a canonical form (minimization)
* The emptiness problem
* The finiteness problem

The use of a ranked alphabet is not assumed.
The emptiness problem is solved by reduction
to reachability in a hypergraph.

Finiteness is reduced to acyclicity of a connection graph.

The visualization is through [Graphviz](https://graphviz.org),
which unfortunately has no support for hypergraphs.
They are simulated here by use of a directed two-coloured graph
where white nodes are actual nodes, and black nodes are hyperedge labels.
Tree acceptors are represented by strongly directed hypergraphs
(a variant which uses sequences rather than sets for sources and sinks),
so this workaround is especially useful
in that sequence indices can be placed as labels
on the edges of the graph.
