{-# OPTIONS -XExistentialQuantification #-}
module Base where

import GHC.Exts ( SpecConstrAnnotation(..) )

-- | We need two constructors here, despite SPEC2 not being used.
-- Otherwise it would be optimised out too early.
data SPEC = SPEC | SPEC2
{-# ANN type SPEC ForceSpecConstr #-}


-- Existential pair of a state and a step function
data Sucker a = forall s. Sucker s (s -> Step s a)

-- A stream can finish, yield a new element and state, or just produce a new state
data Step s a = Done
              | Yield s a
              | Skip s

