module State where

import qualified Data.Map as M
import qualified Data.Set as S
import Types
import Control.Monad.Trans.State (StateT, get)
import Control.Arrow (second)
import Control.Monad.Trans.Class (lift)

-----------------------------------------------------------
--                  DATA
-----------------------------------------------------------

-- Monad type for computations that require access to the immutable problem context
type App m a = StateT Context m a

data Context = Context
    { stations :: S.Set Station
    , connections :: S.Set Connection
    , trains :: S.Set Train
    , passengers :: S.Set Passenger
    } deriving (Show, Eq, Ord)

emptyContext = Context S.empty S.empty S.empty S.empty

data State = State
    { time :: Time
    , trainLocations :: M.Map (ID Train) TrainLocation
    , passengerLocations :: M.Map (ID Passenger) PassengerLocation
    , trainActions :: M.Map (ID Train) [TrainAction]
    , passengerActions :: M.Map (ID Passenger) [PassengerAction]
    } deriving (Show)

-----------------------------------------------------------
--                  ACCESSORS
-----------------------------------------------------------

trainsInStation :: State -> ID Station -> [ID Train]
trainsInStation s s_id = M.keys $ M.filter isAtStation locs where
    locs = trainLocations s
    isAtStation (TLocStation s_id' _) | s_id == s_id' = True
    isAtStation _ = False

trainsInConnection :: State -> ID Connection -> [ID Train]
trainsInConnection s c_id = M.keys $ M.filter isAtStation locs where
    locs = trainLocations s
    isAtStation (TLocConnection c_id' _ _) | c_id == c_id' = True
    isAtStation _ = False

connectionsFrom :: Context -> ID Station -> S.Set (Connection, ID Station)
connectionsFrom c s_id = cs where
    cs = S.map withOtherStation $ S.filter endsAtStation (connections c)
    withOtherStation c@Connection { c_stations = (a,b) }
        | a == s_id = (c, b)
        | otherwise = (c, a)
    endsAtStation c = let (a,b) = c_stations c in a == s_id || b == s_id

-----------------------------------------------------------
--                  Prepare Input
-----------------------------------------------------------

-- | Assigns stations to all pending trains and returns the list of resulting states
setTrainStartPositions :: Context -> [(Context, M.Map (ID Train) [TrainAction])]
setTrainStartPositions c = go c pendingTrainIDs where
    pendingTrainIDs = S.elems $ S.map t_id $ S.filter hasNoStation (trains c)
    go :: Context -> [ID Train] -> [(Context, M.Map (ID Train) [TrainAction])]
    go _ [] = []
    go c (t:ts) = let cs = setTrainStartPosition c t in concatMap (merge ts) cs
    merge [] (c,tas) = [(c,tas)]
    merge ts (c,tas) = let results = go c ts in fmap (second (M.union tas)) results

-- | Returns a list of state where the start station of a train is set to all available stations
setTrainStartPosition :: Context -> ID Train -> [(Context, M.Map (ID Train) [TrainAction])]
setTrainStartPosition c tid = 
    [ (c', tas)
    | station <- S.elems ss
    , let c' = c { trains = modifyTrain tid (\t -> t { start = Just (s_id station) }) ts }
    , let tas = M.singleton tid [Start (s_id station)]
    ]
    where
        ss = stations c
        ts = trains c
        modifyTrain :: ID Train -> (Train -> Train) -> S.Set Train -> S.Set Train
        modifyTrain tid f = S.map (\t -> if tid == t_id t then f t else t)


-----------------------------------------------------------
--                  SCORING
-----------------------------------------------------------
type Score = Double

scoreForState :: State -> Score
scoreForState = const 0

-----------------------------------------------------------
--                  VALIDATION
-----------------------------------------------------------

stateIsValid :: Monad m => State -> App m Bool
stateIsValid s = do
    ss <- stations <$> get
    cs <- connections <$> get
    return $ all stationIsValid ss && all connectionIsValid cs
    where
        stationIsValid station =
            let cap = s_capacity station
                usage = length $ trainsInStation s (s_id station)
            in cap >= usage
        connectionIsValid connection =
            let cap = c_capacity connection
                usage = length $ trainsInConnection s (c_id connection)
            in cap >= usage

-----------------------------------------------------------
--                  MOVING
-----------------------------------------------------------

nextStates :: State -> App [] [State]
nextStates s = do
    ts <- S.elems . trains <$> get
    ps <- S.elems . passengers <$> get
    ss <- moveTrains ts s
    s' <- lift ss
    movePassengers ps s'

moveTrains :: [Train] -> State -> App [] [State]
moveTrains [] s = return []
moveTrains (t:ts) s = do
    ss <- moveTrain t s
    s' <- lift ss
    moveTrains ts s

moveTrain :: Monad m => Train -> State -> App m [State]
moveTrain train s = case tloc M.! tid of
    TLocConnection c_id s_id distance
        -- train is on connection in next round
        | distance > vel -> return [s { trainLocations = M.insert tid (TLocConnection c_id s_id (distance - vel)) tloc }]
        -- train reaches next station at next round
        | otherwise -> return [s { trainLocations = M.insert tid (TLocStation s_id False) tloc }]
    TLocStation s_id _ -> do
        c <- get
        let cs = S.elems $ connectionsFrom c s_id
        let stayingTrain = s { trainLocations = M.insert tid (TLocStation s_id True) tloc }
            leavingTrains = flip fmap cs $
                \(c, s_id') -> if distance c > vel
                    -- train is on connection in next round
                    then s { trainLocations = M.insert tid (TLocConnection (c_id c) s_id' vel) tloc }
                    -- train reaches next station at next round
                    else s { trainLocations = M.insert tid (TLocStation s_id' False) tloc }
        return $ stayingTrain : leavingTrains
    where
        tid = t_id train
        tloc = trainLocations s
        vel = velocity train

movePassengers :: [Passenger] -> State -> App [] [State]
movePassengers [] s = return []
movePassengers (p:ps) s = do
    ss <- movePassenger p s
    s' <- lift ss
    movePassengers ps s'

movePassenger :: Monad m => Passenger -> State -> App m [State]
movePassenger pas s = return $ case ploc M.! pid of
    -- passenger at station -> (stay at station) : [board at train | train <- boardable trains at station]
    PLocStation s_id ->
        let availableTrains = filter (isBoardable . (tloc M.!)) (trainsInStation s s_id)
        in s : fmap (\tid -> s { passengerLocations = M.insert pid (PLocTrain tid) ploc }) availableTrains
    -- passenger at train -> [stay at station, leave if train is boardable]
    PLocTrain t_id -> case tloc M.! t_id of
        TLocStation s_id True -> [s, s { passengerLocations = M.insert pid (PLocStation s_id) ploc }]
        _ -> [s]
    where
        pid = p_id pas
        ploc = passengerLocations s
        tloc = trainLocations s

-----------------------------------------------------------
--                  RESULT CHECKING
-----------------------------------------------------------

-- stateIsFinished :: State -> Bool
-- stateIsFinished State { passengers, passengerLocations } = all isAtDestination passengers  where
--     isAtDestination Passenger { p_id, destination } = passengerLocations M.! p_id == PLocStation destination

stateIsFinished :: Monad m => State -> App m Bool
stateIsFinished s = do
    ps <- passengers <$> get
    return $ all isAtDestination ps
    where
        ploc = passengerLocations s
        isAtDestination Passenger { p_id, destination } = ploc M.! p_id == PLocStation destination