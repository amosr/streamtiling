{-# OPTIONS -XExistentialQuantification #-}
module Buffers where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Control.Monad.ST

data Sucker a = forall s. Sucker s (s -> Step s a)
data Step s a = Done | Yield s a | Skip s


-- ||| Buffers
-- TODO make array
type Buffer a = V.Vector a
bufferLen = 32

{-# INLINE fillBuffer #-}
fillBuffer :: s -> (s -> Step s a) -> Buffer a -> ST s2 (Maybe (Buffer a, s))
fillBuffer s f buf
 = do   buf' <- V.unsafeThaw buf
        case f s of
           Done       -> return Nothing
           Skip  s'   -> goBuffer buf' 0 s' f -- Nothing -- $ go bufferLen     s' []
           Yield s' a -> goBuffer buf' 1 s' f -- $ Just a -- $ go (bufferLen-1) s' [a]

{-
{-# INLINE mkBuffer #-}
mkBuffer :: s -> (s -> Step s a) -> Maybe a -> (Buffer a, s)
mkBuffer s f a = runST $ do
  m <- MV.new bufferLen
  (m',s) <- case a of
   Nothing -> goBuffer m 0 s f
   Just a' -> MV.unsafeWrite m 0 a' >> goBuffer m 1 s f
  v <- V.unsafeFreeze m'
  return (v,s)
-}

{-# INLINE goBuffer #-}
goBuffer m n s f
 = do   (v,s') <- go n s
        v' <- V.unsafeFreeze v
        return (Just (v',s'))
 where
  {-# INLINE go #-}
  go n s
   | n >= MV.length m -- should = bufferLen
   = return (m,s)
  go n s
   = case f s of
     Yield s' a -> MV.unsafeWrite m n a >> go (n+1) s'
     Skip  s'   ->                         go n     s'
     -- NB we are 'allowed' to do a take here because
     -- goBuffer will not be called again
     Done       -> return (MV.unsafeTake n m, s)
     
{-# INLINE initBuffer #-}
initBuffer :: ST s2 (Buffer a, Int)
initBuffer
 = do   v <- MV.new bufferLen
        v'<- V.unsafeFreeze v
        return (v', bufferLen+1)

{-# INLINE bufferise #-}
bufferise :: Sucker a -> s -> (Maybe a -> s -> Step s b) -> Sucker b
bufferise (Sucker s1 f1) s f
 = Sucker (s, Just (s1, runST initBuffer)) next
 where
  {-# INLINE next #-}
  next (s, Just (sB, (v,ix)))
   | ix >= V.length v
   = case runST $ fillBuffer sB f1 v of
     Nothing        -> Skip (s, Nothing)
     Just (buf, sB')-> Skip (s, Just (sB', (buf,0)))
  next (s, Just (sB, (v,ix) ))
   = let a   = v V.! ix
         ix' = ix + 1
     in
     case f (Just a) s of
     Done       -> Done
     Yield s' b -> Yield (s', Just (sB, (v,ix'))) b
     Skip  s'   -> Skip  (s', Just (sB, (v,ix')))
  next (s, Nothing)
   = case f Nothing s of
     Done       -> Done
     Yield s' b -> Yield (s', Nothing) b
     Skip  s'   -> Skip  (s', Nothing)

