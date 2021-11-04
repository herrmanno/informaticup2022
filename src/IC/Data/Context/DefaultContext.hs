module IC.Data.Context.DefaultContext
    ( DefaultContext(..)
    , connectionsFrom
    , setPassengerStartLocations
    , setTrainStartPositions
    , setTrainStartPosition
    , emptyContext
    ) where

import           Control.Arrow         (Arrow ((***)))
import           Data.Foldable         (find)
import qualified Data.Map              as M
import           Data.Maybe            (fromJust, mapMaybe)
import qualified Data.Set              as S
import           IC.Data.Connection    (Connection (..))
import           IC.Data.Context.Class (Context (..))
import           IC.Data.ID            (ID)
import           IC.Data.Passenger     (Passenger (..),
                                        PassengerLocation (PLocStation))
import           IC.Data.State         (PassengerLocations, TrainActions,
                                        TrainLocations)
import           IC.Data.Station       (Station (..))
import           IC.Data.Train         (Train (..), TrainAction (..),
                                        TrainLocation (TLocStation),
                                        TrainStatus (Boardable), hasStation)

-----------------------------------------------------------
--                  DATA
-----------------------------------------------------------

data DefaultContext = DefaultContext
    { _stations    :: S.Set Station
    , _connections :: S.Set Connection
    , _trains      :: S.Set Train
    , _passengers  :: S.Set Passenger
    } deriving (Show, Eq, Ord)

emptyContext :: DefaultContext
emptyContext = DefaultContext S.empty S.empty S.empty S.empty

-- TODO: should handle case where not connection exists between stations
stationDistance :: DefaultContext -> ID Station -> ID Station -> Double
stationDistance c fromStation toStation = go fromStation toStation (S.singleton toStation) where
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

instance Context DefaultContext where
    makeContext = DefaultContext
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
    withOtherStation con@Connection { c_stations = (a,b) }
        | a == s_id = (con, b)
        | otherwise = (con, a)
    endsAtStation con = let (a,b) = c_stations con in a == s_id || b == s_id

-----------------------------------------------------------
--                  Prepare Context
-----------------------------------------------------------

setPassengerStartLocations :: Context c => c -> PassengerLocations
setPassengerStartLocations c = M.fromList $ map f (S.elems $ passengers c) where
    f Passenger { p_id, departure } = (p_id, PLocStation departure)

-- | Assigns stations to all pending trains and returns the list of resulting states
setTrainStartPositions :: Context c => c -> [(TrainLocations, TrainActions)]
setTrainStartPositions c = go trainIDs where
    trainIDs = S.elems $ S.map t_id $ trains c
    go :: [ID Train] -> [(TrainLocations, TrainActions)]
    go []     = []
    go (t:ts) = let cs = setTrainStartPosition c t in concatMap (merge ts) cs
    merge [] (tloc,tas) = [(tloc,tas)]
    merge ts (tloc,tas) = let results = go ts in fmap (M.union tloc *** M.union tas) results

-- | Returns a list of state where the start station of a train is set to all available stations
setTrainStartPosition :: Context c => c -> ID Train -> [(TrainLocations, TrainActions)]
setTrainStartPosition c tid
    | hasStation t = let sid = fromJust (start t) in [ (M.singleton tid (TLocStation sid Boardable), M.empty) ]
    | otherwise =
        [ (tloc, tas)
        | station <- S.elems ss
        , let tloc = M.singleton tid (TLocStation (s_id station) Boardable)
        , let tas = M.singleton tid [Start 0 (s_id station)]
        ]
    where
        t = case find ((==tid) . t_id) (S.elems ts) of
            Just t' -> t'
            _       -> error $ "Bad train id: " <> show tid
        ss = stations c
        ts = trains c
