{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 0
#endif

module Data.Copointed where

import Control.Applicative
import Data.Default.Class
import GHC.Generics

#ifdef MIN_VERSION_comonad
import Control.Comonad.Trans.Env
import Control.Comonad.Trans.Store
import Control.Comonad.Trans.Traced

#if !(MIN_VERSION_comonad(4,3,0))
import Data.Functor.Coproduct
#endif
#endif

#ifdef MIN_VERSION_containers
import Data.Tree
#endif

#ifdef MIN_VERSION_semigroupoids
import Data.Functor.Bind
#endif


#if defined(MIN_VERSION_semigroups) || (MIN_VERSION_base(4,9,0))
import Data.Semigroup as Semigroup
import Data.List.NonEmpty (NonEmpty(..))
#endif

import qualified Data.Monoid as Monoid

#ifdef MIN_VERSION_tagged
import Data.Tagged
#endif

#if defined(MIN_VERSION_transformers) || (MIN_VERSION_base(4,8,0))
import Data.Functor.Identity
#endif

#if defined(MIN_VERSION_transformers) || (MIN_VERSION_base(4,9,0))
import Data.Functor.Sum as F
import Data.Functor.Compose
#endif

#ifdef MIN_VERSION_transformers
import Data.Functor.Reverse
import Control.Applicative.Backwards
import Control.Applicative.Lift as Applicative
import Control.Monad.Trans.Identity
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
#endif

-- | 'Copointed' does not require a 'Functor', as the only relationship
-- between 'copoint' and 'fmap' is given by a free theorem.

class Copointed p where
  copoint :: p a -> a

instance Copointed ((,) a) where
  copoint = snd

instance Copointed ((,,) a b) where
  copoint (_,_,a) = a

instance Copointed ((,,,) a b c) where
  copoint (_,_,_,a) = a

instance Default m => Copointed ((->)m) where
  copoint f = f def

instance Copointed m => Copointed (WrappedMonad m) where
  copoint = copoint . unwrapMonad

#ifdef MIN_VERSION_comonad
instance (Default m, Copointed w) => Copointed (TracedT m w) where
  copoint (TracedT w) = copoint w def

instance Copointed w => Copointed (EnvT e w) where
  copoint = copoint . lowerEnvT

instance Copointed w => Copointed (StoreT s w) where
  copoint (StoreT wf s) = copoint wf s
#endif

#ifdef MIN_VERSION_comonad
#if !(MIN_VERSION_comonad(4,3,0))
instance (Copointed p, Copointed q) => Copointed (Coproduct p q) where
  copoint = coproduct copoint copoint
#endif
#endif

#ifdef MIN_VERSION_containers
instance Copointed Tree where
  copoint = rootLabel
#endif

#ifdef MIN_VERSION_tagged
instance Copointed (Tagged a) where
  copoint = unTagged
#endif

#if defined(MIN_VERSION_transformers) || (MIN_VERSION_base(4,8,0))
instance Copointed Identity where
  copoint = runIdentity
#endif

#if defined(MIN_VERSION_transformers) || (MIN_VERSION_base(4,9,0))
instance (Copointed p, Copointed q) => Copointed (Compose p q) where
  copoint = copoint . copoint . getCompose

instance (Copointed f, Copointed g) => Copointed (F.Sum f g) where
  copoint (F.InL m) = copoint m
  copoint (F.InR m) = copoint m
#endif

#ifdef MIN_VERSION_transformers
instance Copointed f => Copointed (Backwards f) where
  copoint = copoint . forwards

instance Copointed f => Copointed (Applicative.Lift f) where
  copoint (Pure a)   = a
  copoint (Other fa) = copoint fa

instance Copointed f => Copointed (Reverse f) where
  copoint = copoint . getReverse

instance Copointed m => Copointed (IdentityT m) where
  copoint = copoint . runIdentityT

instance Copointed m => Copointed (Lazy.WriterT w m) where
  copoint = fst . copoint . Lazy.runWriterT

instance Copointed m => Copointed (Strict.WriterT w m) where
  copoint = fst . copoint . Strict.runWriterT
#endif

instance Copointed Monoid.Dual where
  copoint = Monoid.getDual

instance Copointed Monoid.Sum where
  copoint = Monoid.getSum

instance Copointed Monoid.Product where
  copoint = Monoid.getProduct

#if defined(MIN_VERSION_semigroups) || (MIN_VERSION_base(4,9,0))
instance Copointed NonEmpty where
  copoint ~(a :| _) = a

instance Copointed Semigroup.First where
  copoint = Semigroup.getFirst

instance Copointed Semigroup.Last where
  copoint = Semigroup.getLast

instance Copointed Semigroup.Max where
  copoint = Semigroup.getMax

instance Copointed Semigroup.Min where
  copoint = Semigroup.getMin

instance Copointed WrappedMonoid where
  copoint = unwrapMonoid
#endif

#ifdef MIN_VERSION_semigroups
#if MIN_VERSION_semigroups(0,16,2)
#define HAVE_ARG 1
#endif
#elif MIN_VERSION_base(4,9,0)
#define HAVE_ARG 1
#endif

#ifdef HAVE_ARG
instance Copointed (Arg a) where
  copoint (Arg _ b) = b
#endif

#ifdef MIN_VERSION_semigroupoids
instance Copointed f => Copointed (WrappedApplicative f) where
  copoint = copoint . unwrapApplicative

instance Copointed f => Copointed (MaybeApply f) where
  copoint (MaybeApply (Left fa)) = copoint fa
  copoint (MaybeApply (Right a)) = a
#endif

instance Copointed Par1 where
  copoint = unPar1

instance Copointed f => Copointed (M1 i c f) where
  copoint = copoint . unM1

instance Copointed f => Copointed (Rec1 f) where
  copoint = copoint . unRec1

instance (Copointed f, Copointed g) => Copointed (f :+: g) where
  copoint (L1 a) = copoint a
  copoint (R1 a) = copoint a

instance (Copointed f, Copointed g) => Copointed (f :.: g) where
  copoint = copoint . copoint . unComp1
