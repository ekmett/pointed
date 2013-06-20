{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module Data.Pointed where

import Control.Arrow
import Control.Applicative
import Control.Concurrent.STM
import Data.Default
import qualified Data.Monoid as Monoid
import Data.Semigroup as Semigroup
import Data.Functor.Identity
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Tree (Tree(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Functor.Constant
import qualified Data.Functor.Product as Functor
import Data.Functor.Compose
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Error
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Data.List.NonEmpty
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import Data.Semigroupoid.Static

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

instance Pointed STM where
  point = return

instance Pointed Tree where
  point a = Node a []

instance Pointed NonEmpty where
  point a = a :| []

instance Pointed ZipList where
  point = pure

instance Pointed Identity where
  point = Identity

instance Pointed ((->)e) where
  point = const

instance Default e => Pointed ((,)e) where
  point = (,) def

instance Monad m => Pointed (WrappedMonad m) where
  point = WrapMonad . return

instance Default m => Pointed (Const m) where
  point _ = Const def

instance Arrow a => Pointed (WrappedArrow a b) where
  point = pure

instance Pointed Dual where
  point = Dual

instance Pointed Endo where
  point = Endo . const

instance Pointed Sum where
  point = Sum

instance Pointed Monoid.Product where
  point = Monoid.Product

instance Pointed Monoid.First where
  point = Monoid.First . Just

instance Pointed Monoid.Last where
  point = Monoid.Last . Just

instance Pointed Semigroup.First where
  point = Semigroup.First

instance Pointed Semigroup.Last where
  point = Semigroup.Last

instance Pointed Semigroup.Max where
  point = Semigroup.Max

instance Pointed Semigroup.Min where
  point = Semigroup.Min

instance Pointed Seq where
  point = Seq.singleton

instance Pointed Set where
  point = Set.singleton

instance (Pointed p, Pointed q) => Pointed (Compose p q) where
  point = Compose . point . point

instance (Pointed p, Pointed q) => Pointed (Functor.Product p q) where
  point a = Functor.Pair (point a) (point a)

instance Default m => Pointed (Constant m) where
  point _ = Constant def

instance Pointed (ContT r m) where
  point a = ContT ($ a)

instance Pointed m => Pointed (ErrorT e m) where
  point = ErrorT . point . Right

instance Pointed m => Pointed (IdentityT m) where
  point = IdentityT . point

instance Pointed m => Pointed (ListT m) where
  point = ListT . point . point

instance Pointed m => Pointed (MaybeT m) where
  point = MaybeT . point . point

instance Pointed m => Pointed (ReaderT r m) where
  point = ReaderT . const . point

instance (Default w, Pointed m) => Pointed (Lazy.RWST r w s m) where
  point a = Lazy.RWST $ \_ s -> point (a, s, def)

instance (Default w, Pointed m) => Pointed (Strict.RWST r w s m) where
  point a = Strict.RWST $ \_ s -> point (a, s, def)

instance (Default w, Pointed m) => Pointed (Lazy.WriterT w m) where
  point a = Lazy.WriterT $ point (a, def)

instance (Default w, Pointed m) => Pointed (Strict.WriterT w m) where
  point a = Strict.WriterT $ point (a, def)

instance Pointed m => Pointed (Lazy.StateT s m) where
  point a = Lazy.StateT $ \s -> point (a, s)

instance Pointed m => Pointed (Strict.StateT s m) where
  point a = Strict.StateT $ \s -> point (a, s)

instance Pointed m => Pointed (Static m a) where
  point = Static . point . const
