Control.Monad.ST2
=================

The Control.Monad.ST2 monad is like the Control.Monad.ST monad, but
with finer-grained control over access to mutable state. The phantom
type parameters r and w are used to track the read and write
dependencies of the computation. If a computation of type ST2 r w a is
polymorphic in w then it does not write any external state. If it is
also polymorphic in r then it does not read any external state.

Change log
----------

0.1.0.0 - First version
