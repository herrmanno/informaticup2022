module App where

import Data.Set (Set)
import Control.Monad.Trans.State (StateT)
import Types.Station (Station)
import Types.Connection (Connection)
import Types.Train (Train)
import Types.Passenger (Passenger)
import Context (Context)

-- Monad type for computations that require access to the immutable problem context
type App m a = forall c. Context c => StateT c m a
