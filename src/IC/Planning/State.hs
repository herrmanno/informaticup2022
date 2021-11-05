{-|
Offers a `State` type describing the train planning problem's state at a fixed point of time
as well as various functions for manipulating such state and deriving following states.
-}
module IC.Planning.State
    (
    -- * Types
      State(..)
    , Score
    -- * Functions
    -- ** Creating a state
    , emptyState
    , fromContext
    -- ** Accessing state information
    , trainsInStation
    , trainsInConnection
    -- ** Calculating a state's score
    , scoreForState
    -- ** Derive new states
    , nextStates
    , moveTrains
    , moveTrain
    , movePassengers
    , movePassenger
    -- ** Validate a state
    , stateIsValid
    , stateIsFinished
    ) where

import           Control.Monad.Trans.Class (lift)
import qualified Data.Map                  as M
import           Data.Maybe                (mapMaybe)
import qualified Data.Set                  as S
import           IC.Control.MonadPlan      (MonadPlan, get)
import           IC.Data.Connection        (Connection (..))
import           IC.Data.Context           (Context (..), connectionsFrom,
                                            setPassengerStartLocations,
                                            setTrainStartPositions)
import           IC.Data.ID                (ID)
import           IC.Data.Passenger         (Passenger (..),
                                            PassengerAction (..),
                                            PassengerLocation (..),
                                            isPLocStation)
import           IC.Data.State             (PassengerActions,
                                            PassengerLocations, TrainActions,
                                            TrainLocations)
import           IC.Data.Station           (Station (..))
import           IC.Data.Train             (Train (..), TrainAction (..),
                                            TrainLocation (..),
                                            TrainStatus (..), isBoardable,
                                            makeBoardable, prepareBoarding)

-----------------------------------------------------------
--                  DATA
-----------------------------------------------------------

-- |A current state of all trains and passengers at a specific point of time
data State = State
    { time               :: Int                 -- ^ point of time
    , trainLocations     :: TrainLocations      -- ^ locations of all trains
    , passengerLocations :: PassengerLocations  -- ^ locations of all passengers
    , trainActions       :: TrainActions        -- ^ actions, executed so far, on all trains
    , passengerActions   :: PassengerActions    -- ^ actions, executed so far, on all passengers
    } deriving (Eq, Ord)

instance Show State where
    show s = unlines
            [ "time: " <> show (time s)
            , "tloc: " <> show (trainLocations s)
            , "ploc: " <> show (passengerLocations s)
            ]

-- |Creates an empty state
emptyState :: State
emptyState = State 0 M.empty M.empty M.empty M.empty

-- |Creates one or multiple states from a given context
--  If there are trains with out a fixed start station this function
--  will return multiple states.
fromContext :: Context c => c -> [State]
fromContext c = map createState ss where
    createState (tloc, tacs) = State 0 tloc ploc tacs M.empty
    ploc = setPassengerStartLocations c
    ss = setTrainStartPositions c

tickTime :: State -> State
tickTime s = s { time = time s + 1 }

-----------------------------------------------------------
--                  ACCESSORS
-----------------------------------------------------------

-- |Return the IDs of all trains resting in a given station
trainsInStation :: State -> ID Station -> [ID Train]
trainsInStation s s_id = M.keys $ M.filter isAtStation locs where
    locs = trainLocations s
    isAtStation (TLocStation s_id' _) | s_id == s_id' = True
    isAtStation _ = False

-- |Return the IDs of all trains resting on a given connection
trainsInConnection :: State -> ID Connection -> [ID Train]
trainsInConnection s c_id = M.keys $ M.filter isOnConnection locs where
    locs = trainLocations s
    isOnConnection (TLocConnection c_id' _ _) | c_id == c_id' = True
    isOnConnection _ = False


-----------------------------------------------------------
--                  SCORING
-----------------------------------------------------------
-- |The type of score a given state has
--  Lower values mean state should have higher priority
type Score = Double

-- |Calculates the score of a given state
scoreForState :: Monad m => State -> MonadPlan m Score
scoreForState s = do
    ps <- S.elems . passengers <$> get
    ts <- S.elems . trains <$> get
    pscores <- mapM (`passengerScore` s) ps
    tscores <- mapM (`trainScore` s) ts
    return $ sum pscores + sum tscores

passengerScore :: Monad m => Passenger -> State -> MonadPlan m Score
passengerScore p s
    | ploc == PLocStation destination = return 0
    | otherwise = do
        c <- get
        let d = distanceBetween c nextStation destination
        let d' = d + nextStationDistance
        let timeFactor = if time < arrival
                then 1 / fromIntegral (arrival - time)
                else fromIntegral (max 1 (time - arrival))
        let trainFactor = if isPLocStation ploc then 0.5 else 0
        return $ trainFactor + fromIntegral size * timeFactor * d'
    where
        State { time, passengerLocations } = s
        Passenger { p_id, destination, arrival, size } = p
        ploc = passengerLocations M.! p_id
        nextStation = nextPassengerStation p_id s
        nextStationDistance = nextPassengerStationDistance p_id s

trainScore :: Monad m => Train -> State -> MonadPlan m Score
trainScore t s = do
    c <- get
    let trainStation = nextTrainStation tid s
        freePassengerLocations = mapMaybe passengerStation (M.elems ploc)
        minPassengerDistance = minimum [distanceBetween c trainStation ps | ps <- trainStation:freePassengerLocations ]
    return $ minPassengerDistance + nextTrainStationDistance tid s
    where
        tid = t_id t
        ploc = passengerLocations s
        passengerStation (PLocStation sid) = Just sid
        passengerStation _                 = Nothing

nextPassengerStation :: ID Passenger -> State -> ID Station
nextPassengerStation pid s = case passengerLocations s M.! pid of
    PLocStation sid -> sid
    PLocTrain tid   -> nextTrainStation tid s

nextTrainStation :: ID Train -> State -> ID Station
nextTrainStation tid s = case trainLocations s M.! tid of
    TLocStation sid _      -> sid
    TLocConnection _ sid _ -> sid

nextPassengerStationDistance :: ID Passenger -> State -> Double
nextPassengerStationDistance pid s = case passengerLocations s M.! pid of
    PLocStation _ -> 0
    PLocTrain tid -> max onTrainPenalty (nextTrainStationDistance tid s)
    where
        onTrainPenalty = 0.01

-- FIXME: does not use train's velocity!
nextTrainStationDistance :: ID Train -> State -> Double
nextTrainStationDistance tid s = case trainLocations s M.! tid of
    TLocStation _ _      -> 0
    TLocConnection _ _ d -> d

-----------------------------------------------------------
--                  VALIDATION
-----------------------------------------------------------

-- |Returns `True` if a state is valid (e.g. no station's or connection's capacity limit is disobeyed)
stateIsValid :: Monad m => State -> MonadPlan m Bool
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

-- |Derives one or multiple states from a given state by moving all trains and passengers
nextStates :: State -> MonadPlan [] [State]
nextStates state = do
    trains <- S.elems . trains <$> get
    passengers <- S.elems . passengers <$> get
    ss <- moveTrains trains (tickTime state)
    s' <- lift ss
    ss' <- movePassengers passengers s'
    return $ fmap makeAllArrivedTrainsBoardable ss'
    where
        makeAllArrivedTrainsBoardable s = s
            { trainLocations = M.map makeBoardable (trainLocations s) }

moveTrains :: [Train] -> State -> MonadPlan [] [State]
moveTrains [] s = return [s]
moveTrains (t:ts) s = do
    ss <- moveTrain t s
    s' <- lift ss
    moveTrains ts s'

moveTrain :: Monad m => Train -> State -> MonadPlan m [State]
moveTrain train s = case tloc M.! tid of
    TLocConnection c_id s_id distance
        -- train is on connection in next round
        | distance > vel -> return [s { trainLocations = M.insert tid (TLocConnection c_id s_id (distance - vel)) tloc }]
        -- train reaches next station at next round
        | otherwise -> return [s { trainLocations = M.insert tid (TLocStation s_id Arriving) tloc }]
    TLocStation s_id _ -> do
        c <- get
        let cs = S.elems $ connectionsFrom c s_id
        let stayingTrain = s
            leavingTrains = flip fmap cs $
                \(con, s_id') ->
                    let tac = Depart (time s) (c_id con)
                    in if distance con > vel
                        -- train is on connection in next round
                        then s
                            { trainLocations = M.insert tid (TLocConnection (c_id con) s_id' vel) tloc
                            , trainActions = M.alter (Just . maybe [tac] (tac:)) tid tacs
                            }
                        -- train reaches next station at next round
                        else s
                            { trainLocations = M.insert tid (TLocStation s_id' Arriving) tloc
                            , trainActions = M.alter (Just . maybe [tac] (tac:)) tid tacs
                            }
        return $ stayingTrain : leavingTrains
    where
        tid = t_id train
        tloc = trainLocations s
        tacs = trainActions s
        vel = velocity train

movePassengers :: [Passenger] -> State -> MonadPlan [] [State]
movePassengers [] s = return [s]
movePassengers (p:ps) s = do
    ss <- movePassenger p s
    s' <- lift ss
    movePassengers ps s'

movePassenger :: Monad m => Passenger -> State -> m [State]
movePassenger pas s = return $ case ploc M.! pid of
    -- passenger at station -> (stay at station) : [board at train | train <- boardable trains at station]
    PLocStation s_id ->
        let availableTrains = filter (isBoardable . (tloc M.!)) (trainsInStation s s_id)
        in s : fmap doBoard availableTrains
    -- passenger at train -> [stay at station, leave if train is boardable]
    PLocTrain t_id -> case tloc M.! t_id of
        TLocStation sid Boardable -> [s, doUnboard sid]
        _                         -> [s]
    where
        pid = p_id pas
        ploc = passengerLocations s
        pacs = passengerActions s
        tloc = trainLocations s
        doBoard tid =
            let pac = Board (time s) tid
            in s
                { passengerLocations = M.insert pid (PLocTrain tid) ploc
                , passengerActions = M.alter (Just . maybe [pac] (pac:)) pid  pacs
                }
        doUnboard sid =
            let pac = Detrain (time s)
            in s
                { passengerLocations = M.insert pid (PLocStation sid) ploc
                , passengerActions = M.alter (Just . maybe [pac] (pac:)) pid  pacs
                }

-----------------------------------------------------------
--                  RESULT CHECKING
-----------------------------------------------------------

-- |Returns `True` if the given state is finished (e.g. all passengers are at destination station)
stateIsFinished :: Monad m => State -> MonadPlan m Bool
stateIsFinished s = do
    ps <- passengers <$> get
    return $ all isAtDestination ps
    where
        ploc = passengerLocations s
        isAtDestination Passenger { p_id, destination } = ploc M.! p_id == PLocStation destination
