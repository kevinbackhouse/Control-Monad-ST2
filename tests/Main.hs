-- Copyright 2013 Kevin Backhouse.

module Main where

import Control.Monad.ST2
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Bits

import Test.Framework as TF ( defaultMain, testGroup, Test )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.QuickCheck as QC
import Test.QuickCheck.Property ( morallyDubiousIOProperty )

-- | 'TestST2' is a trivial wrapper around 'PureST2'. Its only purpose
-- is to define a 'QC.Testable' instance for 'PureST2'.
newtype TestST2 a
  = TestST2 (PureST2 a)

instance QC.Testable a => QC.Testable (TestST2 a) where
  property (TestST2 (PureST2 m)) = morallyDubiousIOProperty (st2ToIO m)

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests =
  [ testGroup "Control.Monad.ST2"
      [ testProperty "CreateWriteRead" prop_CreateWriteRead
      , testProperty "ST2RArray" prop_ST2RArray
      , testProperty "parallelST2" prop_parallelST2
      ]
  ]

-- | This property creates an array, initialises its elements, then
-- reads the elements to check that it worked correctly.
prop_CreateWriteRead :: Int -> TestST2 Bool
prop_CreateWriteRead n0 =
  let n = n0 `mod` 0x100 in
  let ks = [0 .. n-1] in
  TestST2 $ PureST2 $
  do xs <- newST2Array_ (0,n-1)
     sequence_
       [ writeST2Array xs k k
       | k <- ks
       ]
     ks' <- mapM (readST2Array xs) ks
     return (ks == ks')

-- | This property is a demonstration of how a read-only array can be
-- returned through 'readOnlyST2'. The type checker rejects this code if
-- prop_ST2RArray_helper returns an 'ST2Array' rather than an
-- 'ST2RArray'.
prop_ST2RArray :: Int -> TestST2 Bool
prop_ST2RArray n0 =
  let n = n0 `mod` 0x100 in
  let ks = [0 .. n-1] in
  TestST2 $ PureST2 $
  do xs <- readOnlyST2 (prop_ST2RArray_helper n)
     ks' <- mapM (readST2RArray xs) ks
     return (ks == ks')

prop_ST2RArray_helper :: Int -> ST2 r w (ST2RArray r Int Int)
prop_ST2RArray_helper n =
   do xs <- newST2Array_ (0,n-1)
      sequence_
        [ writeST2Array xs i i
        | i <- [0 .. n-1]
        ]
      return (mkST2RArray xs)

prop_parallelST2 :: Int -> TestST2 Bool
prop_parallelST2 k0 =
  let nShift = 12 in
  let k = k0 `mod` (nShift+1) in
  let n = (1 :: Int) `shiftL` nShift in
  TestST2 $ PureST2 $
  do let bnds = (0,n-1)
     xs <- newST2Array_ bnds
     let n' = n `shiftR` k
     let bnds' = (0,n'-1)

     -- Initialise the array, using multiple threads.
     parallelST2 bnds' $ \i ->
       sequence_
         [ let j' = (i `shiftL` k) .|. j in
           writeST2Array xs j' j'
         | j <- [0 .. (1 `shiftL` k) - 1]
         ]

     -- Compute sub-totals, using multiple threads.
     ys <- newST2Array_ bnds'
     parallelST2 bnds' $ \i ->
       do Sum total <-
            execWriterT $
            sequence_
              [ do x <- lift $ readST2Array xs ((i `shiftL` k) .|. j)
                   tell (Sum x)
              | j <- [0 .. (1 `shiftL` k) - 1]
              ]
          writeST2Array ys i total

     -- Sum the sub-totals.
     Sum total <-
       execWriterT $
       sequence_
         [ do y <- lift $ readST2Array ys i
              tell (Sum y)
         | i <- [0 .. n' - 1]
         ]

     -- Check the answer.
     return (total == (n * (n-1)) `div` 2)
