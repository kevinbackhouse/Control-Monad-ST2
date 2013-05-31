-- Copyright 2013 Kevin Backhouse.

{-|
  The 'ST2' monad is like the 'Control.Monad.ST.ST' monad, but with
  finer-grained control over access to mutable state. The phantom type
  parameters @r@ and @w@ are used to track the read and write
  dependencies of the computation. If a computation of type @'ST2' r w
  a@ is polymorphic in w then it does not write any external state. If
  it is also polymorphic in @r@ then it does not read any external
  state.  Operations which modify state, such as 'writeST2Ref', are
  considered to read external state as well as write it, so it is
  impossible for a computation of type @'ST2' r w a@ to be polymorphic
  in @r@ but not in @w@. This means that the @r@ type parameter
  behaves exactly like the @s@ type parameter on the
  'Control.Monad.ST.ST' monad, but the @w@ type parameter provides
  extra information which is not available in the 'Control.Monad.ST.ST'
  monad.

  Like the 'Control.Monad.ST.ST' monad, the 'ST2' monad allows mutable
  references and arrays to be created, read, and written within a
  computation.  Provided that the mutable state does not escape, it
  does not affect the type signature of the top-level computation.
-}

module Control.Monad.ST2
  ( -- * ST2 Monad
    ST2
  , pureST2, PureST2(..)
  , readOnlyST2, ReadOnlyST2(..)
  , ioToST2, st2ToIO

    -- * ST2 References
  , ST2Ref
  , newST2Ref, readST2Ref, writeST2Ref, modifyST2Ref
  , importST2Ref, exportST2Ref

    -- * ST2 Arrays
  , ST2Array
  , newST2Array, newST2Array_
  , readST2Array, writeST2Array
  , boundsST2Array
  , importST2Array, exportST2Array

    -- * ST2 Read-only Arrays
  , ST2RArray
  , mkST2RArray
  , readST2RArray

    -- * Parallelism
  , parallelST2
  )
where

import Control.Exception ( assert, bracket_ )
import Control.Concurrent ( forkIO )
import qualified Control.Concurrent.SSem as S
import Data.Array.IO.Safe
import Data.IORef

-- | The 'ST2' monad is a newtype of 'IO'. The type parameters @r@ and
-- @w@ are phantom type parameters which are used to track the read
-- and write dependencies of the computation.
newtype ST2 r w a
  = ST2 { unwrapST2 :: IO a }
    deriving Functor

instance Monad (ST2 r w) where
  return x = ST2 $ return x

  (ST2 m) >>= f =
    ST2 $
    do x <- m
       unwrapST2 (f x)

  fail msg = ST2 $ fail msg

-- | This function checks that the sub-computation is polymorphic in
-- both type parameters. This means that the sub-computation does not
-- read or write any state from the enclosing context.
pureST2 :: (forall r w. ST2 r w a) -> ST2 r' w' a
pureST2 m = m

-- | This function checks that the sub-computation is polymorphic in
-- the @w@ type parameter. This means that the sub-computation does
-- not write any state from the enclosing context (but read-only
-- operations are permitted).
readOnlyST2 :: (forall w. ST2 r w a) -> ST2 r w' a
readOnlyST2 m = m

-- | 'runPureST2' is semantically equivalent to pureST2, but uses a
-- newtype to package the @forall@. Sometimes this packaging is
-- convenient when passing a value of type 'PureST2' as an argument
-- because it avoids the need for a nested @forall@ in the type
-- signature.
newtype PureST2 a
  = PureST2 { runPureST2 :: forall r w. ST2 r w a }

-- | 'runReadOnlyST2' is semantically equivalent to readOnlyST2, but
-- uses a newtype to package the @forall@. Sometimes this packaging is
-- convenient when passing a value of type 'ReadOnlyST2' as an
-- argument because it avoids the need for a nested @forall@ in the
-- type signature.
newtype ReadOnlyST2 r a
  = ReadOnlyST2 { runReadOnlyST2 :: forall w. ST2 r w a }

-- | 'IO' computations can be converted to 'ST2' computations, but
-- only with a monomorphic type signature.
ioToST2 :: IO a -> ST2 () () a
ioToST2 m = ST2 m

-- | 'ST2' computations can be converted to 'IO' computations, but
-- only with a monomorphic type signature.
st2ToIO :: ST2 () () a -> IO a
st2ToIO (ST2 m) = m

-- | Mutable reference. 'ST2Ref' is actually just a newtype of an
-- 'IORef', but the @r@ and @w@ type parameters allow the read and
-- write dependencies to be tracked by the type system.
newtype ST2Ref r w a
  = ST2Ref (IORef a)

-- | Create a new reference. The @r@ and @w@ type parameters of the
-- reference are unified with the 'ST2' monad to indicate that new
-- state is created in the enclosing context.
{-# INLINE newST2Ref #-}
newST2Ref :: a -> ST2 r w (ST2Ref r w a)
newST2Ref x =
  ST2 $
  do r <- newIORef x
     return (ST2Ref r)

-- | Read a reference. The @w@ type parameter of the reference is not
-- unified with the 'ST2' monad to indicate that this access is
-- read-only.
{-# INLINE readST2Ref #-}
readST2Ref :: ST2Ref r w a -> ST2 r w' a
readST2Ref (ST2Ref r) =
  ST2 $ readIORef r

-- | Write to a reference. The @w@ type parameter of the reference is
-- unified with the 'ST2' monad to indicate that state is written in
-- the enclosing context.
{-# INLINE writeST2Ref #-}
writeST2Ref :: ST2Ref r w a -> a -> ST2 r w ()
writeST2Ref (ST2Ref r) x =
  ST2 $ writeIORef r x

-- | Modify a reference.
{-# INLINE modifyST2Ref #-}
modifyST2Ref :: ST2Ref r w a -> (a -> a) -> ST2 r w ()
modifyST2Ref (ST2Ref r) f =
  ST2 $ modifyIORef r f

-- | Convert an IORef to an ST2Ref.
{-# INLINE importST2Ref #-}
importST2Ref :: IORef a -> ST2Ref () () a
importST2Ref r = ST2Ref r

-- | Convert an ST2Ref to an IORef.
{-# INLINE exportST2Ref #-}
exportST2Ref :: ST2Ref () () a -> IORef a
exportST2Ref (ST2Ref r) = r

-- | Mutable array. 'ST2Array' is actually just a newtype of an
-- | 'IOArray', but the @r@ and @w@ type parameters allow the read and
-- | write dependencies to be tracked by the type system.
newtype ST2Array r w i a
  = ST2Array (IOArray i a)

-- | Create an array with an initial value.
{-# INLINE newST2Array #-}
newST2Array :: Ix i => (i,i) -> a -> ST2 r w (ST2Array r w i a)
newST2Array bnds x =
  ST2 $
  do xs <- newArray bnds x
     return (ST2Array xs)

-- | Create an uninitialised array.
{-# INLINE newST2Array_ #-}
newST2Array_ :: Ix i => (i,i) -> ST2 r w (ST2Array r w i a)
newST2Array_ bnds =
  ST2 $
  do xs <- newArray_ bnds
     return (ST2Array xs)

-- | Read an index of the array. The @w@ type parameter of the
-- reference is not unified with the 'ST2' monad to indicate that this
-- access is read-only.
{-# INLINE readST2Array #-}
readST2Array :: Ix i => ST2Array r w i a -> i -> ST2 r w' a
readST2Array (ST2Array xs) i =
  ST2 $ readArray xs i

-- | Write an index of the array. The @w@ type parameter of the array
-- is unified with the 'ST2' monad to indicate that state is written
-- in the enclosing context.
{-# INLINE writeST2Array #-}
writeST2Array :: Ix i => ST2Array r w i a -> i -> a -> ST2 r w ()
writeST2Array (ST2Array xs) i a =
  ST2 $ writeArray xs i a

-- | Read the size of the array. Neither type parameter is unified with
-- the 'ST2' monad because the array itself is not accessed.
-- (Conceptually, an 'ST2Array' is a pair, consisting of the size
-- information and the array. This function only accesses the size
-- information, which is immutable.)
{-# INLINE boundsST2Array #-}
boundsST2Array :: Ix i => ST2Array r w i a -> ST2 r' w' (i,i)
boundsST2Array (ST2Array xs) =
  ST2 $ getBounds xs

-- | Convert an IOArray to an ST2Array.
{-# INLINE importST2Array #-}
importST2Array :: IOArray i a -> ST2Array () () i a
importST2Array r = ST2Array r

-- | Convert an ST2Array to an IOArray.
{-# INLINE exportST2Array #-}
exportST2Array :: ST2Array () () i a -> IOArray i a
exportST2Array (ST2Array r) = r

-- | Read-only array. Existential quantification is used to hide the
-- @w@ type parameter. This means that it can escape a 'ReadOnlyST2'
-- context, and can be read in the enclosing context. However, it is
-- impossible for anyone to write to the array outside of the
-- 'ReadOnlyST2' context in which the array was created.
data ST2RArray r i a
  = forall w. ST2RArray !(ST2Array r w i a)

-- | Create a read-only array. It is important to note that this
-- function does not make the original 'ST2Array' immutable. It merely
-- creates a read-only reference to the original array. However, the
-- 'ST2RArray' can be returned through a 'readOnlyST2' or
-- 'ReadOnlyST2' context and the typing rules ensure that the original
-- 'ST2Array' cannot be modified outside of the 'ReadOnlyST2' context
-- in which it was created. In other words, the original 'ST2Array'
-- can continue to be modified after the 'ST2RArray' is created, but
-- only until the 'ST2RArray' is returned through a 'ReadOnlyST2'
-- context.
{-# INLINE mkST2RArray #-}
mkST2RArray :: ST2Array r w i a -> ST2RArray r i a
mkST2RArray xs =
  ST2RArray xs

-- | Read an index of the read-only array.
{-# INLINE readST2RArray #-}
readST2RArray :: Ix i => ST2RArray r i a -> i -> ST2 r w a
readST2RArray (ST2RArray xs) i =
  readST2Array xs i


----------------------------------------------------------------------
------------------------------ Threads -------------------------------
----------------------------------------------------------------------

-- | Spawn one thread for each index in the range and wait for all the
-- threads to finish. Each thread is parameterised by its index, which
-- is an element of the range.
parallelST2 :: Ix i => (i,i) -> (i -> ST2 r w ()) -> ST2 r w ()
parallelST2 bnds f =
  let n = rangeSize bnds in
  assert (n > 0) $
  ST2 $
  -- If n == 1 then there is no need to use forkIO.
  if n == 1
     then unwrapST2 (f (fst bnds))
     else do s <- S.new 0
             sequence_
               [ forkIO $
                 bracket_ (return ()) (S.signal s) (unwrapST2 (f i))
               | i <- range bnds
               ]
             S.waitN s n
