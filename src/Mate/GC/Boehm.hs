{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
module Mate.GC.Boehm (
     mallocBytesGC,
     unsafeFreeGC,
     getHeapSizeGC,
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
   mallocGC_c :: CSize -> IO (Ptr a) 

foreign import ccall "gc/gc.h GC_get_heap_size"
   heapSizeGC_c :: IO CSize

foreign import ccall "gc/gc.h GC_init"
   initGC_c :: IO ()

foreign import ccall "gc/gc.h GC_add_roots"
   addRootGCInternal_c :: Ptr a -> Ptr a -> IO ()

-- counts allocated memory in heap
{-# NOINLINE globalBytesAllocated #-}
globalBytesAllocated :: IORef Int
globalBytesAllocated = unsafePerformIO (newIORef 0)

-- |Initializs the GC. Should be called before mallocBytesGC
initGC :: IO ()
initGC = initGC_c

#ifdef DEBUG
-- |Allocates size bytes in managed memory
mallocBytesGC :: Int -> IO (Ptr a)
mallocBytesGC size = do
  --print "trying alloc"
  ptr <- modifyIORef globalBytesAllocated (+ size) >> mallocGC_c (fromIntegral size)
  if ptr == nullPtr 
   then error "mallocBytes asked memory management for memory but returned nullPtr. (see BoehmGC)"
   else return ptr

#else
-- |Allocates size bytes in managed memory
mallocBytesGC :: Int -> IO (Ptr a)
mallocBytesGC = mallocGC_c . fromIntegral
#endif

-- |Explicitely deallocate an object. Not required and dangerous.
unsafeFreeGC :: Ptr a -> IO ()
unsafeFreeGC _ = print "not implemented"

-- |Returns currently allocated memory in bytes
getHeapSizeGC :: IO Int
getHeapSizeGC = liftM fromIntegral heapSizeGC_c

-- |Adds a memory segment to be GC root
addRootGC :: Ptr a -> Ptr a -> IO ()
addRootGC = addRootGCInternal_c
