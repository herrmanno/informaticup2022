module Context
    ( Context(..)
    , connectionsFrom
    , setTrainStartPositions
    , setTrainStartPosition
    -- only exported for testing purposes
    , ContextType(..)
    , emptyContext
    ) where

import qualified Data.Set as S
import qualified Data.Map as M
import Types (ID)
import Types.Station ( Station(..) )
import Types.Connection ( Connection(..) )
import Types.Train ( TrainAction(..), Train(..), hasNoStation )
import Types.Passenger
import Control.Arrow (second)


-----------------------------------------------------------
--                  CLASS
-----------------------------------------------------------

class Context c where
    makeContext :: S.Set Station -> S.Set Connection -> S.Set Train -> S.Set Passenger -> c
    stations :: c -> S.Set Station
    connections :: c -> S.Set Connection
    trains :: c -> S.Set Train
    passengers :: c -> S.Set Passenger

-----------------------------------------------------------
--                  DATA
-----------------------------------------------------------

data ContextType = ContextType
    { _stations :: S.Set Station
    , _connections :: S.Set Connection
    , _trains :: S.Set Train
    , _passengers :: S.Set Passenger
    } deriving (Show, Eq, Ord)

emptyContext :: ContextType
emptyContext = ContextType S.empty S.empty S.empty S.empty

instance Context ContextType where
    makeContext = ContextType
    stations = _stations
    connections = _connections
    trains = _trains
    passengers = _passengers

-----------------------------------------------------------
--                  ACCESSORS
-----------------------------------------------------------

connectionsFrom :: Context c => c -> ID Station -> S.Set (Connection, ID Station)
connectionsFrom c s_id = cs where
    cs = S.map withOtherStation $ S.filter endsAtStation (connections c)
    withOtherStation c@Connection { c_stations = (a,b) }
        | a == s_id = (c, b)
        | otherwise = (c, a)
    endsAtStation c = let (a,b) = c_stations c in a == s_id || b == s_id

-----------------------------------------------------------
--                  Prepare Context
-----------------------------------------------------------

-- | Assigns stations to all pending trains and returns the list of resulting states
setTrainStartPositions :: Context c => c -> [(c, M.Map (ID Train) [TrainAction])]
setTrainStartPositions c = go c pendingTrainIDs where
    pendingTrainIDs = S.elems $ S.map t_id $ S.filter hasNoStation (trains c)
    go :: Context c => c -> [ID Train] -> [(c, M.Map (ID Train) [TrainAction])]
    go _ [] = []
    go c (t:ts) = let cs = setTrainStartPosition c t in concatMap (merge ts) cs
    merge [] (c,tas) = [(c,tas)]
    merge ts (c,tas) = let results = go c ts in fmap (second (M.union tas)) results

-- | Returns a list of state where the start station of a train is set to all available stations
setTrainStartPosition :: Context c => c -> ID Train -> [(c, M.Map (ID Train) [TrainAction])]
setTrainStartPosition c tid = 
    [ (c', tas)
    | station <- S.elems ss
    , let c' = makeContext
            (stations c)
            (connections c)
            (modifyTrain tid (\t -> t { start = Just (s_id station) }) ts)
            (passengers c)
    , let tas = M.singleton tid [Start (s_id station)]
    ]
    where
        ss = stations c
        ts = trains c
        modifyTrain :: ID Train -> (Train -> Train) -> S.Set Train -> S.Set Train
        modifyTrain tid f = S.map (\t -> if tid == t_id t then f t else t)