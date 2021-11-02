{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module App (App, evalApp, get, ContextT) where

import Data.Set (Set)
import Control.Monad.Trans.State (StateT, evalStateT)
import Types.Station (Station)
import Types.Connection (Connection)
import Types.Train (Train)
import Types.Passenger (Passenger)
import Context (Context)
import Control.Monad.Trans.Reader (ReaderT (runReaderT, ReaderT), reader, ask)
import Control.Monad.Trans.Class (lift, MonadTrans)
import Data.Functor.Classes (Eq1 (liftEq))

-- Monad type for computations that require access to the immutable problem context
-- type App m a = forall c. Context c => StateT c m a

-- TODO: remove existential type and use (Context c) constraint on functions instead
type App m a = forall c. Context c => ContextT c m a

newtype ContextT c m a = ContextT { runContextT :: ReaderT c m a }
    deriving (Functor, Applicative, Monad)

instance MonadTrans (ContextT c) where
    lift = ContextT . lift

evalApp :: (Monad m, Context c) => App m a -> c -> m a
-- evalApp = evalStateT
evalApp (ContextT r) = runReaderT r

-- |Fetch the context from the environment
get :: (Monad m, Context c) => ContextT c m c
get = ContextT ask