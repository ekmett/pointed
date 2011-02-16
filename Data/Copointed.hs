module Data.Copointed where

import Control.Comonad
import Data.Default
import Data.Functor.Identity 
import Data.Functor.Compose
import Data.Functor.Coproduct
import Data.Tree
import Data.Monoid as Monoid
import Data.Semigroup as Semigroup
import Control.Monad.Trans.Identity
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Comonad.Trans.Discont.Lazy as Lazy
import qualified Control.Comonad.Trans.Discont.Memo as Memo
import qualified Control.Comonad.Trans.Discont.Strict as Strict
import qualified Control.Comonad.Trans.Env.Lazy as Lazy
import qualified Control.Comonad.Trans.Env.Strict as Strict
import qualified Control.Comonad.Trans.Store.Lazy as Lazy
import qualified Control.Comonad.Trans.Store.Memo as Memo
import qualified Control.Comonad.Trans.Store.Strict as Strict

-- | 'Copointed' does not require a 'Functor', as the only relationship
-- between 'copoint' and 'fmap' is given by a free theorem.

class Copointed p where
  copoint :: p a -> a

instance Copointed Identity where
  copoint = runIdentity

instance Default m => Copointed ((->)m) where
  copoint f = f def 

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

instance Copointed Semigroup.First where
  copoint = Semigroup.getFirst

instance Copointed Semigroup.Last where
  copoint = Semigroup.getLast

instance Copointed Semigroup.Max where
  copoint = Semigroup.getMax

instance Copointed Semigroup.Min where
  copoint = Semigroup.getMin

instance Copointed (Lazy.DiscontT s w) where
  copoint (Lazy.DiscontT f w) = f w

instance Copointed (Strict.DiscontT s w) where
  copoint (Strict.DiscontT f w) = f w

instance Copointed (Memo.DiscontT s w) where
  copoint = extract

instance Copointed w => Copointed (Lazy.EnvT e w) where
  copoint = copoint . Lazy.lowerEnvT

instance Copointed w => Copointed (Strict.EnvT e w) where
  copoint = copoint . Strict.lowerEnvT

instance Copointed w => Copointed (Lazy.StoreT s w) where
  copoint (Lazy.StoreT wf s) = copoint wf s

instance Copointed w => Copointed (Strict.StoreT s w) where
  copoint (Strict.StoreT wf s) = copoint wf s

instance Copointed w => Copointed (Memo.StoreT s w) where
  copoint = copoint . Memo.lowerStoreT 
