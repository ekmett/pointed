{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif
-------------------------------------------------------------------------------
-- |
-- Module      :  Data.Pointed.Generic
-- Copyright   :  (C) 2015 Edward Kmett, Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  MPTCs
--
-- This module provides generic deriving tools for 'Pointed' structures.
--
-- This approach is loosely based off of the @data-default-generics@ library,
-- written by João Cristóvão.
--
-------------------------------------------------------------------------------
module Data.Pointed.Generic
  ( GPointed(..)
  , genericPoint
  ) where

import Data.Default.Class
import Data.Pointed
import Data.Proxy
import GHC.Generics

-- | Class of pointed generic representation types.
class GPointed rep f where
    gpoint :: proxy (rep x) -- ^ Carries the underlying data type as @rep@
           -> p             -- ^ The value to be pointed
           -> f p           -- ^ The generic pointed representation value

instance GPointed rep U1 where
    gpoint _ _ = U1

instance GPointed rep Par1 where
    gpoint _ = Par1

instance Default c => GPointed rep (K1 i c) where
    gpoint _ _ = K1 def

instance GPointed rep f => GPointed rep (M1 i c f) where
    gpoint proxy = M1 . gpoint proxy

instance (HasRecPointed rep f, GPointed rep f, GPointed rep g) =>
  GPointed rep (f :+: g) where
    gpoint proxy a = let lPoint = gpoint proxy a
                         rPoint = gpoint proxy a
                      in if hasRecPointed proxy lPoint
                            then R1 rPoint
                            else L1 lPoint

instance (GPointed rep f, GPointed rep g) => GPointed rep (f :*: g) where
    gpoint proxy a = gpoint proxy a :*: gpoint proxy a

instance Pointed f => GPointed rep (Rec1 f) where
    gpoint _ = Rec1 . point

instance (Pointed f, GPointed rep g) => GPointed rep (f :.: g) where
    gpoint proxy = Comp1 . point . gpoint proxy

-- | Generically generate a `point` operation for any type implementing 'Generic1'.
-- This operation will look for the leftmost data constructor with which to point the
-- value. If possible, 'genericPoint' will skip constructors that contain direct data
-- recursion (since that will cause this function to never terminate), but if all
-- constructors have direct recursion, then 'genericPoint' will pick the rightmost one.
genericPoint :: forall f a. (Generic1 f, GPointed f (Rep1 f)) => a -> f a
genericPoint a = to1 $ gpoint (Proxy :: Proxy (f x)) a

-- | Detects whether a generic structure contains direct recursion (e.g.,
--
-- @
-- data Recur1 a = Recur1 (Recur1 a)
-- data Recur2 a = Recur2 [Recur2 a]
-- data Recur3 a = Recur3 (Recur3 [a])
-- @
--
-- all contain direct recursion.)
--
-- This is used to avoid selecting a directly recursive data constructor in a
-- ':+:' sum, since calling 'genericPoint' with that constructor would never
-- terminate.
--
-- This trick originally appeared in João Cristóvão's @data-default-generics@
-- library.
class HasRecPointed rep f where
    hasRecPointed :: proxy (rep x) -> f p -> Bool
    hasRecPointed _ _ = False

instance HasRecPointed rep V1
instance HasRecPointed rep U1
instance HasRecPointed rep Par1
instance HasRecPointed rep (K1 i c)

instance HasRecPointed rep f => HasRecPointed rep (M1 i c f) where
    hasRecPointed proxy (M1 a) = hasRecPointed proxy a

instance (HasRecPointed rep f, HasRecPointed rep g) => HasRecPointed rep (f :+: g) where
    hasRecPointed proxy (L1 a) = hasRecPointed proxy a
    hasRecPointed proxy (R1 a) = hasRecPointed proxy a

instance (HasRecPointed rep f, HasRecPointed rep g) => HasRecPointed rep (f :*: g) where
    hasRecPointed proxy (a :*: b) = hasRecPointed proxy a || hasRecPointed proxy b

-- We need OverlappingInstances in the cases below to determine whether a generic
-- representation type contains the data type itself, which indicates the presence of
-- direct recursion.

instance
#if __GLASGOW_HASKELL__ >= 710
  {-# OVERLAPPABLE #-}
#endif
  HasRecPointed rep (Rec1 f)

instance
#if __GLASGOW_HASKELL__ >= 710
  {-# OVERLAPPING #-}
#endif
  HasRecPointed f   (Rec1 f) where
    hasRecPointed _ _ = True

instance
#if __GLASGOW_HASKELL__ >= 710
  {-# OVERLAPS #-}
#endif
  HasRecPointed rep g => HasRecPointed rep (f :.: g) where
    -- This looks dangerous, but the only types that @g p@ can take on are @Rec1 f@
    -- or @(f :.: g)@, and we do not need to inspect the value of the generic
    -- representation in either case.
    hasRecPointed proxy _ = hasRecPointed proxy (undefined :: g p)

instance
#if __GLASGOW_HASKELL__ >= 710
  {-# OVERLAPS #-}
#endif
  HasRecPointed f (f :.: g) where
    hasRecPointed _ _ = True

instance
#if __GLASGOW_HASKELL__ >= 710
  {-# OVERLAPS #-}
#endif
  HasRecPointed g (f :.: g) where
    hasRecPointed _ _ = True

instance
#if __GLASGOW_HASKELL__ >= 710
  {-# OVERLAPS #-}
#endif
  HasRecPointed f (f :.: f) where
    hasRecPointed _ _ = True
