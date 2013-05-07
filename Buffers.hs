-- | Functions for buffering a stream.
-- The most important function is 'bufferise', which takes a 'Sucker' and a modified step function
-- and returns a new 'Sucker', buffering the first and calling the step function for each element.
{-# OPTIONS -XBangPatterns #-}
module Buffers where

import Base

import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

import Control.Monad.ST

-- | This really ought to be an unboxed vector.
type Buffer a = V.Vector a
-- | Default buffer length
bufferLen = 32

-- | Some initial state, a step function, and a buffer to fill.
-- If the stream is finished, returns 'Nothing'.
-- Otherwise fills the incoming buffer with up to 'bufferLen' elements.
-- The returned buffer is the same as the incoming buffer, except
-- if there are fewer than 'bufferLen' elements in the stream.
-- In that case it is a prefix ('unsafeTake') of buf.
{-# INLINE fillBuffer #-}
fillBuffer :: Unbox a => s -> (s -> Step s a) -> Buffer a -> Maybe (Buffer a, s)
fillBuffer s f !buf
 -- Pull once before attempting to fill the buffer: the stream may be finished.
 =      case f s of
           Done       -> Nothing
           Skip  s'   -> go Nothing  s'
           Yield s' a -> go (Just a) s'
 where
  -- We must be careful to use 'runST' on the smallest possible computation;
  -- otherwise we may lose valuable optimisations since 'runST' won't be inlined.
  {-# INLINE go #-}
  go !a s' = Just $ runST $ do
                !buf' <- V.unsafeThaw buf
                let i = case a of
                         Nothing -> 0
                         Just _  -> 1
                case a of
                 Nothing  -> return ()
                 Just !a' -> MV.unsafeWrite buf' 0 a'
                goBuffer buf' i s' f


{-# INLINE goBuffer #-}
goBuffer !m !n !s !f
 = do   (len,s') <- go SPEC n s
        -- We can shorten the buffer by using `take':
        -- if len < bufferLen, that means the underlying stream is finished,
        -- and the next call to fill will return `Nothing'.
        -- Otherwise len=bufferLen and it's a noop.
        v' <- V.unsafeFreeze (MV.unsafeTake len m)
        return (v',s')
 where
  {-# INLINE go #-}
  go SPEC !n !s
   -- MV.length m should be equal to bufferLen
   | n >= MV.length m
   = return (n,s)
  go SPEC !n !s
   = case f s of
      Yield s' !a -> MV.unsafeWrite m n a >> go SPEC (n+1) s'
      Skip  s'    ->                         go SPEC n     s'
      Done        -> return (n, s)


{-# INLINE initBuffer #-}
initBuffer :: Unbox a => ST s2 (Buffer a)
initBuffer
 = do   v <- MV.new bufferLen
        V.unsafeFreeze v


-- | Produce a new Sucker:
-- pull from the first Sucker 'bufferLen' elements at a time,
-- then apply each element in the buffer to the given stepper function @f@.
-- Once there are no elements left in the buffered stream,
-- repeatedly step with @f Nothing@ until the stream is done.
--
-- I expect most stream functions will actually finish as soon as the first stream is finished.
-- Append would not, but there is little point buffering append.
{-# INLINE bufferise #-}
bufferise :: Unbox a => Sucker a -> s -> (Maybe a -> s -> Step s b) -> Sucker b
bufferise (Sucker s1 f1) s f
 -- Start with bufferLen+1 as the index, so the buffer is done.
 = Sucker (s, Just (s1, (runST initBuffer, bufferLen+1))) next
 where
  {-# INLINE next #-}
  -- Refill the buffer once we're past the end
  next (s, Just (sB, (v,ix)))
   | ix >= V.length v
   = case fillBuffer sB f1 v of
     Nothing        -> Skip (s, Nothing)
     Just (buf, sB')-> Skip (s, Just (sB', (buf,0)))

  -- Full buffer, take elements and call stepper
  next (s, Just (sB, (v,ix) ))
   = let a   = v `V.unsafeIndex` ix
         ix' = ix + 1
     in
     case f (Just a) s of
     Done       -> Done
     Yield s' b -> Yield (s', Just (sB, (v,ix'))) b
     Skip  s'   -> Skip  (s', Just (sB, (v,ix')))

  -- Empty buffer, finish off the stream.
  next (s, Nothing)
   = case f Nothing s of
     Done       -> Done
     Yield s' b -> Yield (s', Nothing) b
     Skip  s'   -> Skip  (s', Nothing)

