{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module Data.Copointed where

import Data.Default.Class
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Functor.Coproduct
#if MIN_VERSION_transformers(0,4,0)
import qualified Data.Functor.Sum as F
#endif
import Data.Tree
import Data.Semigroup as Semigroup
import Control.Monad.Trans.Identity
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Control.Comonad.Trans.Env
import Control.Comonad.Trans.Store
import Control.Comonad.Trans.Traced
import Data.List.NonEmpty (NonEmpty(..))
import Data.Tagged

-- | 'Copointed' does not require a 'Functor', as the only relationship
-- between 'copoint' and 'fmap' is given by a free theorem.

class Copointed p where
  copoint :: p a -> a

#if MIN_VERSION_transformers(0,4,0)
instance (Copointed f, Copointed g) => Copointed (F.Sum f g) where
  copoint (F.InL m) = copoint m
  copoint (F.InR m) = copoint m
#endif

instance Copointed (Tagged a) where
  copoint = unTagged

instance Copointed Identity where
  copoint = runIdentity

instance Default m => Copointed ((->)m) where
  copoint f = f def

instance (Default m, Copointed w) => Copointed (TracedT m w) where
  copoint (TracedT w) = copoint w def

instance Copointed ((,) a) where
  copoint = snd

instance Copointed ((,,) a b) where
  copoint (_,_,a) = a

instance Copointed ((,,,) a b c) where
  copoint (_,_,_,a) = a

instance Copointed Tree where
  copoint = rootLabel

instance (Copointed p, Copointed q) => Copointed (Compose p q) where
  copoint = copoint . copoint . getCompose

instance (Copointed p, Copointed q) => Copointed (Coproduct p q) where
  copoint = coproduct copoint copoint

instance Copointed m => Copointed (IdentityT m) where
  copoint = copoint . runIdentityT

instance Copointed m => Copointed (Lazy.WriterT w m) where
  copoint = fst . copoint . Lazy.runWriterT

instance Copointed m => Copointed (Strict.WriterT w m) where
  copoint = fst . copoint . Strict.runWriterT

instance Copointed Dual where
  copoint = getDual

instance Copointed Sum where
  copoint = getSum

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

instance Copointed w => Copointed (EnvT e w) where
  copoint = copoint . lowerEnvT

instance Copointed w => Copointed (StoreT s w) where
  copoint (StoreT wf s) = copoint wf s
