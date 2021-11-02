module Context
    ( Context(..)
    , connectionsFrom
    , setPassengerStartLocations
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
import Types.Train ( TrainAction(..), Train(..), hasNoStation, TrainLocation (TLocStation), hasStation )
import Types.Passenger ( Passenger (..), PassengerLocation (PLocStation) )
import Control.Arrow (second, Arrow ((***)))
import Data.Bifunctor (Bifunctor(bimap))
import Data.Foldable (find)
import Data.Maybe (mapMaybe)
import Types.State (PassengerLocations, TrainLocations, TrainActions)


-----------------------------------------------------------
--                  CLASS
-----------------------------------------------------------

class Context c where
    makeContext :: S.Set Station -> S.Set Connection -> S.Set Train -> S.Set Passenger -> c
    stations :: c -> S.Set Station
    connections :: c -> S.Set Connection
    trains :: c -> S.Set Train
    passengers :: c -> S.Set Passenger
    distanceBetween :: c -> ID Station -> ID Station -> Double

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

-- TODO: should handle case where not connection exists between stations
stationDistance :: ContextType -> ID Station -> ID Station -> Double
stationDistance c from to = go from to (S.singleton to) where
    go from to visited
        | from == to = 0
        | otherwise =
            let ns = filter ((`S.notMember` visited) . fst) (neighbours to)
                inf = fromIntegral (maxBound @Int)
            in minimum $ inf : map (\(over, cost) -> cost + go from over (S.insert over visited)) ns
    neighbours sid =
        let f (Connection _ (a,b) _ d)
                | a == sid = Just (b, d)
                | b == sid = Just (a, d)
                | otherwise = Nothing
        in mapMaybe f (S.elems $ _connections c)

instance Context ContextType where
    makeContext = ContextType
    stations = _stations
    connections = _connections
    trains = _trains
    passengers = _passengers
    distanceBetween c a b = stationDistance c a b

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

setPassengerStartLocations :: Context c => c -> PassengerLocations
setPassengerStartLocations c = M.fromList $ map f (S.elems $ passengers c) where
    f Passenger { p_id, departure } = (p_id, PLocStation departure)

-- | Assigns stations to all pending trains and returns the list of resulting states
setTrainStartPositions :: Context c => c -> [(TrainLocations, TrainActions)]
setTrainStartPositions c = go c trainIDs where
    trainIDs = S.elems $ S.map t_id $ trains c
    pendingTrainIDs = S.elems $ S.map t_id $ S.filter hasNoStation (trains c)
    go :: Context c => c -> [ID Train] -> [(TrainLocations, TrainActions)]
    go _ [] = []
    go c (t:ts) = let cs = setTrainStartPosition c t in concatMap (merge ts) cs
    merge [] (tloc,tas) = [(tloc,tas)]
    merge ts (tloc,tas) = let results = go c ts in fmap (M.union tloc *** M.union tas) results

-- | Returns a list of state where the start station of a train is set to all available stations
setTrainStartPosition :: Context c => c -> ID Train -> [(TrainLocations, TrainActions)]
setTrainStartPosition c tid 
    | hasStation t = let Just sid = start t in [ (M.singleton tid (TLocStation sid True), M.empty) ]
    | otherwise =
        [ (tloc, tas)
        | station <- S.elems ss
        , let tloc = M.singleton tid (TLocStation (s_id station) True)
        , let tas = M.singleton tid [Start 0 (s_id station)]
        ]
    where
        Just t = find ((==tid) . t_id) (S.elems ts)
        ss = stations c
        ts = trains c
        modifyTrain :: ID Train -> (Train -> Train) -> S.Set Train -> S.Set Train
        modifyTrain tid f = S.map (\t -> if tid == t_id t then f t else t)