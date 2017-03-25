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
-- Module      :  Data.Copointed.Generic
-- Copyright   :  (C) 2015 Edward Kmett, Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  MPTCs
--
-- This module provides generic deriving tools for 'Copointed' structures.
--
-- This approach is based directly off of the @Generics.Deriving.Copoint@
-- module from José Pedro Magalhães's @generic-deriving@ library, with some
-- tricks borrowed from João Cristóvão's @data-default-generics@ library.
--
-------------------------------------------------------------------------------
module Data.Copointed.Generic
  ( GCopointed(..)
  , genericCopoint
  , genericCopointMay
  ) where

import Data.Copointed
import Data.Proxy
import GHC.Generics

-- | Class of copointed generic representation types.
class GCopointed rep f where
    -- | Provides a default implementation of 'copoint'.
    gcopoint :: proxy (rep x) -- ^ Carries the underlying data type as @rep@
             -> f p           -- ^ The generic representation type
             -> Maybe p       -- ^ @'Just' x@ if @x@ can be found in a hole, 'Nothing' otherwsie

instance GCopointed rep U1 where
    gcopoint _ U1 = Nothing

instance GCopointed rep Par1 where
    gcopoint _ (Par1 a) = Just a

instance GCopointed rep (K1 i c) where
    gcopoint _ _ = Nothing

instance GCopointed rep f => GCopointed rep (M1 i c f) where
    gcopoint proxy (M1 a) = gcopoint proxy a

instance (GCopointed rep f, GCopointed rep g) => GCopointed rep (f :+: g) where
    gcopoint proxy (L1 a) = gcopoint proxy a
    gcopoint proxy (R1 a) = gcopoint proxy a

instance (HasRecCopointed rep f, GCopointed rep f, GCopointed rep g) =>
  GCopointed rep (f :*: g) where
    gcopoint proxy (a :*: b) =
        let fstPoint = gcopoint proxy a
            sndPoint = gcopoint proxy b
         in if hasRecCopointed proxy a
               then sndPoint
               else case fstPoint of
                         Just x  -> Just x
                         Nothing -> sndPoint

instance Copointed f => GCopointed rep (Rec1 f) where
    gcopoint _ (Rec1 a) = Just $ copoint a

instance (Copointed f, GCopointed rep g) => GCopointed rep (f :.: g) where
    gcopoint proxy (Comp1 a) = gcopoint proxy $ copoint a

-- | Generically generate a `copoint` operation for any type implementing 'Generic1'.
-- This operation will look for the leftmost "hole" (i.e., occurence of the last type
-- parameter) from which to pluck a value. If possible, 'genericCopoint' will skip
-- holes that contain direct data recursion (since that might cause this function to
-- never terminate), but if all holes have direct recursion, then 'genericCopoint'
-- will pick the rightmost one.
--
-- If 'genericCopoint' cannot find any holes, then it will fail with an error message.
genericCopoint :: forall f a. (Generic1 f, GCopointed f (Rep1 f)) => f a -> a
genericCopoint a = case genericCopointMay a of
    Just x  -> x
    Nothing -> error "Data type is not copointed"

-- | Like 'genericCopoint', except that if no holes can be found, then 'Nothing' is
-- returned instead of failing with an error message.
genericCopointMay :: forall f a. (Generic1 f, GCopointed f (Rep1 f)) => f a -> Maybe a
genericCopointMay = gcopoint (Proxy :: Proxy (f x)) . from1

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
-- This is used to avoid selecting a directly recursive data record in a ':*:'
-- product, since calling 'genericCopoint' with that might never terminate.
--
-- This trick originally appeared in João Cristóvão's @data-default-generics@
-- library.
class HasRecCopointed rep f where
    hasRecCopointed :: proxy (rep x) -> f p -> Bool
    hasRecCopointed _ _ = False

instance HasRecCopointed rep V1
instance HasRecCopointed rep U1
instance HasRecCopointed rep Par1
instance HasRecCopointed rep (K1 i c)

instance HasRecCopointed rep f => HasRecCopointed rep (M1 i c f) where
    hasRecCopointed proxy (M1 a) = hasRecCopointed proxy a

instance (HasRecCopointed rep f, HasRecCopointed rep g) =>
  HasRecCopointed rep (f :*: g) where
    hasRecCopointed proxy (a :*: b) = hasRecCopointed proxy a && hasRecCopointed proxy b

-- We need OverlappingInstances in the cases below to determine whether a generic
-- representation type contains the data type itself, which indicates the presence of
-- direct recursion.

instance
#if __GLASGOW_HASKELL__ >= 710
  {-# OVERLAPPABLE #-}
#endif
  HasRecCopointed rep (Rec1 f)

instance
#if __GLASGOW_HASKELL__ >= 710
  {-# OVERLAPPING #-}
#endif
  HasRecCopointed f (Rec1 f) where
    hasRecCopointed _ _ = True

instance
#if __GLASGOW_HASKELL__ >= 710
  {-# OVERLAPS #-}
#endif
  HasRecCopointed rep g => HasRecCopointed rep (f :.: g) where
    -- This looks dangerous, but the only types that @g p@ can take on are @Rec1 f@
    -- or @(f :.: g)@, and we do not need to inspect the value of the generic
    -- representation in either case.
    hasRecCopointed proxy _ = hasRecCopointed proxy (undefined :: g p)

instance
#if __GLASGOW_HASKELL__ >= 710
  {-# OVERLAPS #-}
#endif
  HasRecCopointed f (f :.: g) where
    hasRecCopointed _ _ = True

instance
#if __GLASGOW_HASKELL__ >= 710
  {-# OVERLAPS #-}
#endif
  HasRecCopointed g (f :.: g) where
    hasRecCopointed _ _ = True

instance
#if __GLASGOW_HASKELL__ >= 710
  {-# OVERLAPS #-}
#endif
  HasRecCopointed f (f :.: f) where
    hasRecCopointed _ _ = True
