{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 0
#endif

module Data.Pointed where

import Control.Arrow
import Control.Applicative
import qualified Data.Monoid as Monoid
import Data.Default.Class

#ifdef MIN_VERSION_comonad
import Control.Comonad
#endif

#ifdef MIN_VERSION_containers
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq, ViewL(..), ViewR(..))
import qualified Data.Sequence as Seq
import Data.Tree (Tree(..))
#endif

#ifdef MIN_VERSION_kan_extensions
import Data.Functor.Day.Curried
#endif

#if defined(MIN_VERSION_semigroups) || (MIN_VERSION_base(4,9,0))
import Data.Semigroup as Semigroup
import Data.List.NonEmpty
#endif

#ifdef MIN_VERSION_semigroupoids
import Data.Functor.Bind
import Data.Semigroupoid.Static
#endif

#ifdef MIN_VERSION_stm
import Control.Concurrent.STM
#endif

#if defined(MIN_VERSION_transformers) || (MIN_VERSION_base(4,8,0))
import Data.Functor.Identity
#endif

#if defined(MIN_VERSION_transformers) || (MIN_VERSION_base(4,9,0))
import Data.Functor.Compose
import qualified Data.Functor.Product as Functor
#endif

#ifdef MIN_VERSION_transformers
import Data.Functor.Constant
import Data.Functor.Reverse
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import Control.Applicative.Backwards
import Control.Applicative.Lift
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Error
import Control.Monad.Trans.Except
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
#endif

#if defined(MIN_VERSION_tagged) || (MIN_VERSION_base(4,7,0))
import Data.Proxy
#endif

#ifdef MIN_VERSION_tagged
import Data.Tagged
#endif

#if defined(MIN_VERSION_unordered_containers)
import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
#endif

class Pointed p where
  point :: a -> p a

instance Pointed [] where
  point a = [a]

instance Pointed Maybe where
  point = Just

instance Pointed (Either a) where
  point = Right

instance Pointed IO where
  point = return

instance Pointed ZipList where
  point = pure

#if MIN_VERSION_base(4,8,0) || defined(MIN_VERSION_transformers)
instance Pointed Identity where
  point = Identity
#endif

instance Pointed ((->)e) where
  point = const

instance Default e => Pointed ((,)e) where
  point = (,) def

instance Default m => Pointed (Const m) where
  point _ = Const def

instance Monad m => Pointed (WrappedMonad m) where
  point = WrapMonad . return

instance Arrow a => Pointed (WrappedArrow a b) where
  point = pure

instance Pointed Monoid.Dual where
  point = Monoid.Dual

instance Pointed Monoid.Endo where
  point = Monoid.Endo . const

instance Pointed Monoid.Sum where
  point = Monoid.Sum

instance Pointed Monoid.Product where
  point = Monoid.Product

instance Pointed Monoid.First where
  point = Monoid.First . Just

instance Pointed Monoid.Last where
  point = Monoid.Last . Just

#ifdef MIN_VERSION_comonad
instance Pointed (Cokleisli w a) where
  point = Cokleisli . const
#endif

#ifdef MIN_VERSION_containers
instance Pointed Tree where
  point a = Node a []

instance Default k => Pointed (Map k) where
  point = Map.singleton def

instance Pointed Seq where
  point = Seq.singleton

instance Pointed ViewL where
  point a = a :< Seq.empty

instance Pointed ViewR where
  point a = Seq.empty :> a

instance Pointed Set where
  point = Set.singleton
#endif

#ifdef MIN_VERSION_kan_extensions
instance (Functor g, g ~ h) => Pointed (Curried g h) where
  point a = Curried (fmap ($a))
  {-# INLINE point #-}
#endif

#ifdef MIN_VERSION_semigroupoids
instance Pointed m => Pointed (Static m a) where
  point = Static . point . const

instance Pointed f => Pointed (WrappedApplicative f) where
  point = WrapApplicative . point

instance Pointed (MaybeApply f) where
  point = MaybeApply . Right
#endif

#if defined(MIN_VERSION_semigroups) || (MIN_VERSION_base(4,9,0))
instance Pointed NonEmpty where
  point a = a :| []

instance Pointed Semigroup.First where
  point = Semigroup.First

instance Pointed Semigroup.Last where
  point = Semigroup.Last

instance Pointed Semigroup.Max where
  point = Semigroup.Max

instance Pointed Semigroup.Min where
  point = Semigroup.Min

instance Pointed Option where
  point = Option . Just

instance Pointed WrappedMonoid where
  point = WrapMonoid
#endif

#ifdef MIN_VERSION_semigroups
#if MIN_VERSION_semigroups(0,16,2)
#define HAVE_ARG 1
#endif
#elif MIN_VERSION_base(4,9,0)
#define HAVE_ARG 1
#endif

#ifdef HAVE_ARG
instance Default a => Pointed (Arg a) where
  point = Arg def
#endif

#ifdef MIN_VERSION_stm
instance Pointed STM where
  point = return
#endif

#if defined(MIN_VERSION_tagged) || (MIN_VERSION_base(4,7,0))
instance Pointed Proxy where
  point _ = Proxy
#endif

#ifdef MIN_VERSION_tagged
instance Pointed (Tagged a) where
  point = Tagged
#endif

#if defined(MIN_VERSION_transformers) || (MIN_VERSION_base(4,9,0))
instance (Pointed p, Pointed q) => Pointed (Compose p q) where
  point = Compose . point . point
#endif

#if defined(MIN_VERSION_transformers) || (MIN_VERSION_base(4,9,0))
instance (Pointed p, Pointed q) => Pointed (Functor.Product p q) where
  point a = Functor.Pair (point a) (point a)
#endif

#ifdef MIN_VERSION_transformers
instance Pointed (ContT r m) where
  point a = ContT ($ a)

instance Pointed m => Pointed (ErrorT e m) where
  point = ErrorT . point . Right

instance Pointed m => Pointed (ExceptT e m) where
  point = ExceptT . point . Right

instance Pointed m => Pointed (IdentityT m) where
  point = IdentityT . point

instance Pointed m => Pointed (ListT m) where
  point = ListT . point . point

instance Pointed m => Pointed (MaybeT m) where
  point = MaybeT . point . point

instance Pointed m => Pointed (ReaderT r m) where
  point = ReaderT . const . point

instance Default m => Pointed (Constant m) where
  point _ = Constant def

instance Pointed m => Pointed (Lazy.StateT s m) where
  point a = Lazy.StateT $ \s -> point (a, s)

instance Pointed m => Pointed (Strict.StateT s m) where
  point a = Strict.StateT $ \s -> point (a, s)

instance (Default w, Pointed m) => Pointed (Lazy.RWST r w s m) where
  point a = Lazy.RWST $ \_ s -> point (a, s, def)

instance (Default w, Pointed m) => Pointed (Strict.RWST r w s m) where
  point a = Strict.RWST $ \_ s -> point (a, s, def)

instance (Default w, Pointed m) => Pointed (Lazy.WriterT w m) where
  point a = Lazy.WriterT $ point (a, def)

instance (Default w, Pointed m) => Pointed (Strict.WriterT w m) where
  point a = Strict.WriterT $ point (a, def)

instance Pointed f => Pointed (Reverse f) where
  point = Reverse . point

instance Pointed f => Pointed (Backwards f) where
  point = Backwards . point

instance Pointed (Lift f) where
  point = Pure
#endif

#if defined(MIN_VERSION_unordered_containers)
instance (Default k, Hashable k) => Pointed (HashMap k) where
  point = HashMap.singleton def
#endif

