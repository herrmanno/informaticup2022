module IC.Control.MonadPlan (MonadPlan, MonadPlanT, evalPlan, get) where

import           Control.Monad.Trans.Class  (MonadTrans, lift)
import           Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT), ask)
import           IC.Data.Context.Class      (Context)

-- TODO: remove existential type and use (Context c) constraint on functions instead
-- | Monad type for computations that require access to the planning context
type MonadPlan m a = forall c. Context c => MonadPlanT c m a

-- | Monad type for computations that require access to the immutable problem context
newtype MonadPlanT c m a = MonadPlanT (ReaderT c m a)
    deriving (Functor, Applicative, Monad)

instance MonadTrans (MonadPlanT c) where
    lift = MonadPlanT . lift

evalPlan :: Context c => MonadPlanT c m a -> c -> m a
evalPlan (MonadPlanT r) = runReaderT r

-- |Fetch the context from the environment
get :: Monad m => MonadPlanT c m c
get = MonadPlanT ask
