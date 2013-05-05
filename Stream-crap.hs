{-# OPTIONS -XMultiParamTypeClasses -XFlexibleInstances -XFlexibleContexts -XTypeFamilies -XRankNTypes -XExistentialQuantification -XGADTs -XTypeOperators -XUndecidableInstances -XDataKinds #-}


data Z
data S n

data TT
data FF

class (TNum a, TNum b, TNum (TAdd a b)) => Add a b where
 type TAdd a b

--type family (TNum a, TNum b) => Add a b
instance TNum b => Add Z b where
 type TAdd Z b = b
instance (TNum a, TNum b, TNum (TAdd a b)) => Add (S a) b where
 type TAdd (S a) b = S (TAdd a b)

class LtDec a b where
 ltdec :: a -> b -> Bool
instance LtDec Z Z where
 ltdec _ _ = False
instance LtDec Z (S b) where
 ltdec _ _ = True
instance LtDec (S a) Z where
 ltdec _ _ = False
instance LtDec a b => LtDec (S a) (S b) where
 ltdec _ _ = ltdec (undefined :: a) (undefined :: b)


class TNum n where
 intOfNum :: n -> Int
instance TNum Z where
 intOfNum _ = 0
instance TNum n => TNum (S n) where
 intOfNum _ = (intOfNum (undefined :: n)) + 1

{-
instance Add a b => TNum (TAdd a b) where
 intOfNum _ = (intOfNum (undefined :: a)) + (intOfNum (undefined :: b))
-}

type NRegisters = S (S (S (S (S Z))))


-- Function from a to b, with hint about required registers
data F a b n = F (a -> b)

data Gen b n     = Gen (F Int b n)
data Map b n s   = Map (F (TyOf s) b n) s
data Zip s1 s2   = Zip s1 s2
data Filter n s  = Filter (F (TyOf s) Bool n) s
data Concat s a  = (TyOf (TyOf s) ~ a) => Concat s

--data Str sub n where
-- Map :: F a b n -> 


class (TNum (SizeOf a)) => TSizeOf a where
 type SizeOf a
instance TNum n => TSizeOf (F a b n) where
 type SizeOf (F a b n) = n
instance TNum n => TSizeOf (Gen b n) where
 type SizeOf (Gen b n) = n
instance (TNum n, TNum (SizeOf s), TNum (n `TAdd` SizeOf s)) => TSizeOf (Map b n s) where
 type SizeOf (Map b n s) = n `TAdd` SizeOf s
instance (TNum n, TNum (n `TAdd` SizeOf s)) => TSizeOf (Filter n s) where
 type SizeOf (Filter n s) = n `TAdd` SizeOf s
instance (TNum (SizeOf s)) => TSizeOf (Concat s a) where
 type SizeOf (Concat s a) = SizeOf s
instance (TNum (SizeOf s1 `TAdd` SizeOf s2 `TAdd` (S Z))) => TSizeOf (Zip s1 s2) where
 type SizeOf (Zip s1 s2) = SizeOf s1 `TAdd` SizeOf s2 `TAdd` (S Z)

class (TNum (SizeOf s)) => STREAM s where
 type TyOf s
 mkSuck :: s -> Sucker (TyOf s)

data Sucker a = forall s. Sucker s (s -> Step s a)
data Step s a = Done | Yield s a | Skip s

instance TNum n => STREAM (Gen b n) where
 type TyOf (Gen b n)    = b

instance (TNum n, TNum (TAdd n (SizeOf s))) => STREAM (Map b n s) where
 type TyOf (Map b n s)  = b

instance (STREAM s1, STREAM s2,
   LtDec (SizeOf s1 `TAdd` SizeOf s2) NRegisters,
   TNum (TAdd (TAdd (SizeOf s1) (SizeOf s2)) (S Z))) => STREAM (Zip s1 s2) where
 type TyOf (Zip s1 s2)  = (TyOf s1, TyOf s2)
 mkSuck (Zip s1 s2) =
    if   ((undefined :: SizeOf s1 `TAdd` SizeOf s2) `ltdec` (undefined :: NRegisters))
    then mkZipper (mkSuck s1) (mkSuck s2) -- do both at once        
    else bufferZip (mkSuck s1) (mkSuck s2)

mkZipper (Sucker s1 f1) (Sucker s2 f2)
 = Sucker (s1,s2,Nothing) f
 where
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

bufferZip s1 (Sucker s2 f2)
 = Sucker (s2,initBuffer) f
 where
  f (s,[])
   = case fillBuffer s1 of
     Nothing  -> Done
     Just buf -> Skip (s,buf)
  f (s, a:as)
   = case f2 s of
     Done       -> Done
     Yield s' b -> Yield (s', as) (a,b)
     Skip  s'   -> Skip  (s', a:as)


-- TODO make array
type Buffer a = [a]
bufferLen = 32

initBuffer = []

fillBuffer :: Sucker a -> Maybe (Buffer a)
fillBuffer (Sucker s f)
 = case f s of
   Done       -> Nothing
   Skip  s'   -> Just $ reverse $ go bufferLen     s' []
   Yield s' a -> Just $ reverse $ go (bufferLen-1) s' [a]
 where
  go 0 _ acc
   = acc
  go n s' acc
   = case f s of
     Done       -> acc
     Yield s' a -> go (n-1) s' (a:acc)
     Skip  s'   -> go n     s'    acc

instance STREAM s => STREAM (Filter n s) where
 type TyOf (Filter n s) = TyOf s
instance STREAM s => STREAM (Concat s a) where
 type TyOf (Concat s a) = a


type Stream a = forall s. (TyOf s ~ a) => s

