# Notice

I consider this library deprecated in favor of
[haggle](https://github.com/travitch/haggle), which is more efficient
and has better documentation.

# Introduction

This library is an attempt at revising (and documenting) the classic
functional graph library (fgl) for Haskell.  The main contributions
over fgl are:

 * The monolithic interface is split into several independent
   interfaces.  Different implementations need only implement the
   interfaces that are sensible for them.

 * The interface uses
   [associated types](http://www.haskell.org/haskellwiki/GHC/Type_families)
   to parameterize node types, node label types, and edge label types.
   This means that Node is not restricted to just Int anymore.  It
   also means that graph types do not need to be of kind * -> *
   (though they can be).

 * Most of the interface functions are now class members.  Most of
   them have default implementations, but those can be overridden if
   the underlying graph representation can perform them more
   efficiently. (That is, the overriding can be done without the GHC
   rule pragmas found in the original implementation of the
   PatriciaTree).  Only functions need to be implemented to cover the
   entire API (fgl only requires 10 -- parity is probably possible).

 * This package also contains an implementation of the graph interface
   based on the original PatriciaTree, but with parameterized edge
   storage.  That is, edges can be stored in lists, sets, hash sets,
   or hash maps.  Benchmarks forthcoming.  A significant advantage of
   using structures besides lists is that graphs have canonical forms
   and can be compared efficiently.  Lists vs. sets also provide
   alternatives for handling multi-graphs.

At the end of the day, the interface itself is mostly compatible with
fgl.  Instead of using `Gr` directly, choose a more specific type
(`LGraph` should be equivalent).

## Interfaces ##

Its main contribution is breaking the monolithic graph interface
presented by fgl into smaller independent interfaces.  Ideally,
implementations of graphs will implement only the interfaces that make
sense for them.  The sub-interfaces are inspired by the concepts in
the Boost Graph Library with the names and types modified to make more
sense for Haskell.

I am not set on the current interfaces and may collapse a few.  The
biggest advantage of splitting the interface is that interesting graph
variants with no natural (or efficient) implementations of some
methods are now possible.  For example, graphs that store their edges
using unboxed vectors are not easily decomposable via `match`, but are
very space efficient and useful for immutable graphs.  Additionally,
graphs that do not track predecessors can be useful when building
large graphs and not removing any nodes or edges.

## Implementation ##

This package currently contains a parameterized implementation of most
of the graph interfaces based on the original PatriciaTree from fgl.
This implementation allows for different choices of edge storage.
Unfortunately the type signatures of the implementation are a bit
scary (it even uses `-XUndecidableInstances`).  I would like to clean
it up eventually, but at least clients are not affected.
