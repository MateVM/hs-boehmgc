{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
module Mate.GC.Boehm (
     mallocBytesGC,
     unsafeFreeGC,
     getHeapSize,
     initGC,
     addRootGC,
  ) where

#define DEBUG

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Data.IORef
import System.IO.Unsafe
import Foreign.C.Types
import Control.Monad

foreign import ccall "gc/gc.h GC_malloc"
   mallocGC :: CSize -> IO (Ptr a) 

foreign import ccall "gc/gc.h GC_get_heap_size"
   heapSizeGC :: IO CSize


foreign import ccall "gc/gc.h GC_init"
   initGC :: IO ()


foreign import ccall "gc/gc.h GC_add_roots"
   addRootGCInternal :: Ptr a -> Ptr a -> IO ()

-- counts allocated memory in heap
{-# NOINLINE globalBytesAllocated #-}
globalBytesAllocated :: IORef Int
globalBytesAllocated = unsafePerformIO (newIORef 0)

#ifdef DEBUG
-- |Allocates size bytes in managed memory
mallocBytesGC :: Int -> IO (Ptr a)
mallocBytesGC size = do
  --print "trying alloc"
  ptr <- modifyIORef globalBytesAllocated (+ size) >> mallocGC (fromIntegral size)
  if ptr == nullPtr 
   then error "alloc 0"
   else return ptr

#else
-- |Allocates size bytes in managed memory
mallocBytesGC :: Int -> IO (Ptr a)
mallocBytesGC = mallocGC . fromIntegral
#endif

-- |Explicitely deallocate an object. Not required and dangerous.
unsafeFreeGC :: Ptr a -> IO ()
unsafeFreeGC _ = print "not implemented"

-- |Returns currently allocated memory in bytes
getHeapSize :: IO Int
getHeapSize = liftM fromIntegral heapSizeGC 

addRootGC :: Ptr a -> Ptr a -> IO ()
addRootGC = addRootGCInternal 
