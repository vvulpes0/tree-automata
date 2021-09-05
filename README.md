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
