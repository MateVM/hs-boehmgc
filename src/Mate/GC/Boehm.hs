{-# LANGUAGE CPP #-}
module Mate.GC.Boehm (
     mallocBytesGC,
     unsafeFreeGC,
     getHeapSize,
  ) where

#define DEBUG

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Data.IORef
import System.IO.Unsafe

-- counts allocated memory in heap
{-# NOINLINE globalBytesAllocated #-}
globalBytesAllocated :: IORef Int
globalBytesAllocated = unsafePerformIO (newIORef 0)

#ifdef DEBUG
-- |Allocates size bytes in managed memory
mallocBytesGC :: Int -> IO (Ptr a)
mallocBytesGC size = modifyIORef globalBytesAllocated (+ size) >> mallocBytes size
#else
-- |Allocates size bytes in managed memory
mallocBytesGC :: Int -> IO (Ptr a)
mallocBytesGC = mallocBytes
#endif

-- |Explicitely deallocate an object. Not required and dangerous.
unsafeFreeGC :: Ptr a -> IO ()
unsafeFreeGC _ = print "not implemented"

-- |Returns currently allocated memory in bytes
getHeapSize :: IO Int
getHeapSize = readIORef globalBytesAllocated
