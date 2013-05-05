{-# OPTIONS -XTypeFamilies -XRankNTypes -XExistentialQuantification #-}
module Stream where
import Magic
import Buffers


import Prelude hiding (map, filter, zip, (++))

data Stream a = forall s. (STREAM s, TyOf s ~ a) => Stream s

{-# NOINLINE[0] stream #-}
stream :: [a] -> Stream a
stream as = Stream (fromList as)

{-# INLINE[1] unstream #-}
unstream :: Stream a -> [a]
unstream (Stream s) = unstream' s

{-# INLINE unstream' #-}
unstream' :: (STREAM s, TyOf s ~ a) => s -> [a]
unstream' s
 = unfold $ snd $ mkSuck s

{-# INLINE unfold #-}
unfold (Sucker s f) = go s
 where
  {-# INLINE go #-}
  go s
   = case f s of
     Done -> []
     Skip s' -> go s'
     Yield s' a -> a : go s'


{-# RULES
 "unstream_stream" forall x.
   unstream (stream x) = x;

 "stream_unstream" forall (x :: Stream a).
   stream (unstream x) = x
 #-}
{-

 "unstream'_fromList" forall x.
   unstream' (fromList x) = x;

 "fromList_unstream'" forall x.
   fromList (unstream' x) = x;

 These rules don't work because the types contain too much information...
-}
{-# INLINE f_hint #-}
f_hint :: Size
f_hint = 1

{-# INLINE mkF #-}
mkF :: (a -> b) -> F a b
mkF f = F f f_hint

{-# INLINE map #-}
map :: (a -> b) -> [a] -> [b]
map f as = go (stream as)
 where
  {-# INLINE go #-}
  go (Stream s)
   = unstream $ Stream $ Map (mkF f) s

{-# INLINE filter #-}
filter :: (a -> Bool) -> [a] -> [a]
filter f as = go (stream as)
 where
  {-# INLINE go #-}
  go (Stream s)
   = unstream $ Stream $ Filter (mkF f) s

{-# INLINE zip #-}
zip :: [a] -> [b] -> [(a,b)]
zip as bs = go (stream as) (stream bs)
 where
  {-# INLINE go #-}
  go :: Stream a -> Stream b -> [(a,b)]
  go (Stream sa) (Stream sb)
   = unstream $ Stream $ Zip sa sb

{-# INLINE (++) #-}
(++) :: [a] -> [a] -> [a]
(++) as bs = go (stream as) (stream bs)
 where
  {-# INLINE go #-}
  go :: Stream a -> Stream a -> [a]
  go (Stream sa) (Stream sb)
   = unstream $ Stream $ Append sa sb

{-# INLINE gen #-}
gen :: (Int -> a) -> Int -> [a]
gen f i = unstream $ Stream $ Gen (mkF f) i


