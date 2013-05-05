{-# OPTIONS -XTypeFamilies #-}
module Magic where
import Buffers


-- type NRegisters = 4


type Size = Int
{-# INLINE max_registers #-}
max_registers :: Size
max_registers = 7

-- Function from a to b, with hint about required registers
data F a b = F (a -> b) Size

data Gen b      = Gen (F Int b) Int
data Map b  s   = Map (F (TyOf s) b) s
data Zip s1 s2  = Zip s1 s2
data Filter s   = Filter (F (TyOf s) Bool) s
--data Concat s a  = (TyOf (TyOf s) ~ a) => Concat s
data Append s1 s2 = Append s1 s2

--data Str sub n where
-- Map :: F a b n -> 

class STREAM s where
 type TyOf s
 mkSuck :: s -> (Size, Sucker (TyOf s))

instance STREAM (Gen b) where
 type TyOf   (Gen b)  = b
 {-# INLINE mkSuck #-}
 mkSuck (Gen (F f sz) len)
  = (sz+1, Sucker 0 go)
   where
    {-# INLINE go #-}
    go i = if   i < len
           then Yield (i+1) (f i)
           else Done

instance (STREAM s) => STREAM (Map b s) where
 type TyOf (Map b s)  = b
-- type SizeOf (Map b n s) = n
 {-# INLINE mkSuck #-}
 mkSuck (Map (F f sz) s)
  = let (sz', suck') = mkSuck s
    in  if sz + sz' > max_registers
        then (sz+1,   mapBuffer f suck')
        else (sz+sz', mapInline f suck')

{-# INLINE mapBuffer #-}
mapBuffer f suck
 = bufferise suck () f'
 where
  {-# INLINE f' #-}
  f' Nothing  _ = Done
  f' (Just a) s = Yield s (f a)

{-# INLINE mapInline #-}
mapInline f (Sucker s2 f2)
 = Sucker s2 next
 where
  {-# INLINE next #-}
  next s
   = case f2 s of
     Done       -> Done
     Yield s' a -> Yield s' (f a)
     Skip  s'   -> Skip  s'

instance (STREAM s1, STREAM s2) => STREAM (Zip s1 s2) where
 type TyOf (Zip s1 s2)  = (TyOf s1, TyOf s2)
-- type SizeOf (Zip s1 s2) = SizeOf s1 + SizeOf s2
 {-# INLINE mkSuck #-}
 mkSuck (Zip s1 s2) =
    let (sz1,su1) = mkSuck s1
        (sz2,su2) = mkSuck s2
    in  if   sz1 + sz2 > max_registers
        then (sz2+1,   bufferZip su1 su2)
        else (sz1+sz2, mkZipper  su1 su2)  -- do both at once        

{-# INLINE mkZipper #-}
mkZipper (Sucker s1 f1) (Sucker s2 f2)
 = Sucker (s1,s2,Nothing) f
 where
  {-# INLINE f #-}
  f (sa,sb,Nothing)
   = case f1 sa of
     Done       -> Done
     Yield s' a -> Skip (s', sb, Just a)
     Skip  s'   -> Skip (s', sb, Nothing)
  f (sa,sb,Just a)
   = case f2 sb of
     Done       -> Done
     Yield s' b -> Yield (sa, s', Nothing) (a,b)
     Skip  s'   -> Skip  (sa, s', Just a)

{-# INLINE bufferZip #-}
bufferZip sB (Sucker s2 f2)
 = bufferise sB s2 f'
 where
  {-# INLINE f' #-}
  f' Nothing _  = Done
  f' (Just a) s
   = case f2 s of
     Done       -> Done
     Yield s' b -> Yield s' (a,b)
     Skip  s'   -> Skip  s'

{-
bufferZip (Sucker s1 f1) (Sucker s2 f2)
 = Sucker (s1,s2,initBuffer) f
 where
  f (s1,s2,[])
   = case fillBuffer s1 f1 of
     Nothing  -> Done
     Just (buf,s1') -> Skip (s1',s2,buf)
  f (s1,s2, a:as)
   = case f2 s2 of
     Done       -> Done
     Yield s' b -> Yield (s1, s', as) (a,b)
     Skip  s'   -> Skip  (s1, s', a:as)
-}

instance (STREAM s) => STREAM (Filter s) where
 type TyOf (Filter s) = TyOf s
 {-# INLINE mkSuck #-}
 mkSuck (Filter (F f sz) s)
  = let (sz',su') = mkSuck s
    in  if   (sz+sz') > max_registers
        then (sz+1,   filterBuffer f su')
        else (sz+sz', filterInline f su')

{-# INLINE filterBuffer #-}
filterBuffer f suck
 = bufferise suck () f'
 where
  {-# INLINE f' #-}
  f' Nothing  _ = Done
  f' (Just x) s
   = case f x of
     True  -> Yield s x
     False -> Skip  s
{-
filterBuffer f (Sucker s2 f2)
 = Sucker (s2,initBuffer) next
 where
  next (s,[])
   = case fillBuffer s f2 of
     Nothing       -> Done
     Just (buf,s') -> Skip (s', buf)
  next (s, a:as)
   = case f a of
     True  -> Yield (s,as) a
     False -> Skip  (s,as)
-}

{-# INLINE filterInline #-}
filterInline f (Sucker s2 f2)
 = Sucker s2 next
 where
  {-# INLINE next #-}
  next s
   = case f2 s of
     Done      -> Done
     Yield s' a-> if   f a
                  then Yield s' a
                  else Skip  s'
     Skip  s'  -> Skip s'

instance (STREAM s1, STREAM s2, TyOf s1 ~ TyOf s2) => STREAM (Append s1 s2) where
 type TyOf (Append s1 s2) = TyOf s1
 {-# INLINE mkSuck #-}
 mkSuck (Append s1 s2)
  = let (sz1,su1) = mkSuck s1
        (sz2,su2) = mkSuck s2
        -- no point buffering here
    in  (sz1 `max` sz2, appendInline su1 su2)

{-# INLINE appendInline #-}
appendInline (Sucker s1 f1) (Sucker s2 f2)
 = Sucker (Left s1) next
 where
  {-# INLINE next #-}
  next (Left s)
   = case f1 s of
     Done      -> Skip (Right s2)
     Yield s' a-> Yield (Left s') a
     Skip  s'  -> Skip  (Left s')
  next (Right s)
   = case f2 s of
     Done      -> Done
     Yield s' a-> Yield (Right s') a
     Skip  s'  -> Skip  (Right s')
   



-- Unstream from lists
data FromList a = FromList [a]
{-# INLINE fromList #-}
fromList = FromList

instance STREAM (FromList a) where
 type TyOf   (FromList a)  = a
 {-# INLINE mkSuck #-}
 mkSuck (FromList as)
  = (1, Sucker as go)
   where
    {-# INLINE go #-}
    go []     = Done
    go (a:as) = Yield as a



