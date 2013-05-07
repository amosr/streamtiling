-- | Public interface for fusion/fission, hiding the extra goop we need behind the scenes.
{-# OPTIONS -XTypeFamilies -XRankNTypes -XExistentialQuantification -XGADTs #-}
module Stream where
import Base
import Magic


import Prelude hiding (map, filter, zip, (++))
import Data.Vector.Unboxed (Unbox)

-- | Hide the actual implementation type of our Stream
data Stream a = forall s. (STREAM s, TyOf s ~ a) => Stream s

-- | Convert a list to a Stream: delicate inlining because of rewrite rules
{-# NOINLINE[0] stream #-}
stream :: Unbox a => [a] -> Stream a
stream as = Stream (fromList as)

-- | Convert a Stream to a list: delicate inlining because of rewrite rules
{-# INLINE[1] unstream #-}
unstream :: Stream a -> [a]
unstream (Stream s) = unstream' s

-- | Convert a concrete STREAM to a list
{-# INLINE unstream' #-}
unstream' :: (STREAM s, TyOf s ~ a) => s -> [a]
unstream' s
 = unfold $ snd $ mkSuck s

{-# INLINE unfold #-}
unfold (Sucker s f) = go SPEC s
 where
  {-# INLINE go #-}
  go SPEC s
   = case f s of
     Done -> []
     Skip s' -> go SPEC s'
     Yield s' a -> a : go SPEC s'


-- The rules must operate on Streams because we don't care about the implementation type.
{-# RULES
 "unstream_stream" forall x.
   unstream (stream x) = x;

 "stream_unstream" forall (x :: Stream a).
   stream (unstream x) = x
 #-}

-- | Assume any arbitrary function has size 1.
{-# INLINE f_hint #-}
f_hint :: Size
f_hint = 1

{-# INLINE mkF #-}
mkF :: (a -> b) -> F a b
mkF f = F f f_hint

-- | Convert the input list to a Stream, Map over it, then back to a list.
-- Requires some putzing around because we need to expose stream/unstream to the rules,
-- instead of the more obvious implementation.
{-# INLINE map #-}
map :: (Unbox a, Unbox b) => (a -> b) -> [a] -> [b]
map f as = go (stream as)
 where
  {-# INLINE go #-}
  go (Stream s)
   = unstream $ Stream $ Map (mkF f) s

{-# INLINE filter #-}
filter :: (Unbox a) => (a -> Bool) -> [a] -> [a]
filter f as = go (stream as)
 where
  {-# INLINE go #-}
  go (Stream s)
   = unstream $ Stream $ Filter (mkF f) s

{-# INLINE zip #-}
zip :: (Unbox a, Unbox b) => [a] -> [b] -> [(a,b)]
zip as bs = go (stream as) (stream bs)
 where
  {-# INLINE go #-}
  go :: Stream a -> Stream b -> [(a,b)]
  go (Stream sa) (Stream sb)
   = unstream $ Stream $ Zip sa sb

{-# INLINE (++) #-}
(++) :: (Unbox a) => [a] -> [a] -> [a]
(++) as bs = go (stream as) (stream bs)
 where
  {-# INLINE go #-}
  go :: Stream a -> Stream a -> [a]
  go (Stream sa) (Stream sb)
   = unstream $ Stream $ Append sa sb

{-# INLINE gen #-}
gen :: (Unbox a) => (Int -> a) -> Int -> [a]
gen f i = unstream $ Stream $ Gen (mkF f) i


