{-|
Utility Monad for computations in the planning problem domain
-}
module IC.Control.MonadPlan
    (
    -- * Types
      MonadPlan
    , MonadPlanT
    -- * Functions
    , evalPlan
    , get
    ) where

import           Control.Monad.Trans.Class  (MonadTrans, lift)
import           Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT), ask)
import           IC.Data.Context.Class      (Context)

-- |Monad type for computations that require access to the planning context
type MonadPlan m a = forall c. Context c => MonadPlanT c m a

-- |Monad type for computations that require access to the problem's context
newtype MonadPlanT c m a = MonadPlanT (ReaderT c m a)
    deriving (Functor, Applicative, Monad)

instance MonadTrans (MonadPlanT c) where
    lift = MonadPlanT . lift

-- |Evaluate a computation in the planning problem domain
evalPlan :: Context c => MonadPlanT c m a -> c -> m a
evalPlan (MonadPlanT r) = runReaderT r

-- |Fetch the context from the environment
get :: Monad m => MonadPlanT c m c
get = MonadPlanT ask
