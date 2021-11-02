module State where

import qualified Data.Map as M
import qualified Data.Set as S
import App (App, get)
import Context (Context(..), connectionsFrom, setTrainStartPosition, setTrainStartPositions, setPassengerStartLocations)
import Types (ID)
import Types.Station (Station(..))
import Types.Connection (Connection(..))
import Types.Train (Train(..), TrainLocation(..), TrainAction(..), isBoardable)
import Types.Passenger (Passenger(..), PassengerLocation(..), PassengerAction(..), isPLocStation)
import Types.State
import Control.Monad.Trans.Class (lift)
import Data.Maybe (mapMaybe)

-----------------------------------------------------------
--                  DATA
-----------------------------------------------------------

data State = State
    { time :: Int
    , trainLocations :: TrainLocations
    , passengerLocations :: PassengerLocations
    , trainActions :: TrainActions
    , passengerActions :: PassengerActions
    } deriving (Eq, Ord)

instance Show State where
    show s = unlines
            [ "time: " <> show (time s)
            , "tloc: " <> show (trainLocations s)
            , "ploc: " <> show (passengerLocations s)
            ]

emptyState :: State
emptyState = State 1 M.empty M.empty M.empty M.empty

fromContext :: Context c => c -> [State]
fromContext c = map createState ss where
    createState (tloc, tacs) = State 1 tloc ploc tacs M.empty
    ploc = setPassengerStartLocations c
    ss = setTrainStartPositions c

tickTime :: State -> State
tickTime s = s { time = time s + 1 }

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


-----------------------------------------------------------
--                  SCORING
-----------------------------------------------------------
type Score = Double

scoreForState :: Monad m => State -> App m Score
scoreForState s = do
    ps <- S.elems . passengers <$> get
    ts <- S.elems . trains <$> get
    pscores <- mapM (`passengerScore` s) ps
    tscores <- mapM (`trainScore` s) ts
    return $ sum pscores + sum tscores

passengerScore :: Monad m => Passenger -> State -> App m Score
passengerScore p s = do
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

trainScore :: Monad m => Train -> State -> App m Score
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
        passengerStation _ = Nothing

nextPassengerStation :: ID Passenger -> State -> ID Station
nextPassengerStation pid s = case passengerLocations s M.! pid of
    PLocStation sid -> sid
    PLocTrain tid -> nextTrainStation tid s

nextTrainStation :: ID Train -> State -> ID Station
nextTrainStation tid s = case trainLocations s M.! tid of
    TLocStation sid _ -> sid
    TLocConnection _ sid _ -> sid

nextPassengerStationDistance :: ID Passenger -> State -> Double
nextPassengerStationDistance pid s = case passengerLocations s M.! pid of
    PLocStation sid -> 0
    PLocTrain tid -> max onTrainPenalty (nextTrainStationDistance tid s)
    where
        onTrainPenalty = 0.01

-- FIXME: does not use train's velocity!
nextTrainStationDistance :: ID Train -> State -> Double
nextTrainStationDistance tid s = case trainLocations s M.! tid of
    TLocStation sid _ -> 0
    TLocConnection _ _ d -> d

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
    fmap (fmap tickTime) (movePassengers ps s')

moveTrains :: [Train] -> State -> App [] [State]
moveTrains [] s = return [s]
moveTrains (t:ts) s = do
    ss <- moveTrain t s
    s' <- lift ss
    moveTrains ts s'

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
                \(c, s_id') ->
                    let tac = Depart (time s) (c_id c)
                    in if distance c > vel
                        -- train is on connection in next round
                        then s
                            { trainLocations = M.insert tid (TLocConnection (c_id c) s_id' vel) tloc
                            , trainActions = M.alter (Just . maybe [tac] (tac:)) tid tacs
                            }
                        -- train reaches next station at next round
                        else s
                            { trainLocations = M.insert tid (TLocStation s_id' False) tloc
                            , trainActions = M.alter (Just . maybe [tac] (tac:)) tid tacs
                            }
        return $ stayingTrain : leavingTrains
    where
        tid = t_id train
        tloc = trainLocations s
        tacs = trainActions s
        vel = velocity train

movePassengers :: [Passenger] -> State -> App [] [State]
movePassengers [] s = return [s]
movePassengers (p:ps) s = do
    ss <- movePassenger p s
    s' <- lift ss
    movePassengers ps s'

movePassenger :: Monad m => Passenger -> State -> App m [State]
movePassenger pas s = return $ case ploc M.! pid of
    -- passenger at station -> (stay at station) : [board at train | train <- boardable trains at station]
    PLocStation s_id ->
        let availableTrains = filter (isBoardable . (tloc M.!)) (trainsInStation s s_id)
        in s : fmap doBoard availableTrains
    -- passenger at train -> [stay at station, leave if train is boardable]
    PLocTrain t_id -> case tloc M.! t_id of
        TLocStation sid True -> [s, doUnboard sid]
        _ -> [s]
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

stateIsFinished :: Monad m => State -> App m Bool
stateIsFinished s = do
    ps <- passengers <$> get
    return $ all isAtDestination ps
    where
        ploc = passengerLocations s
        isAtDestination Passenger { p_id, destination } = ploc M.! p_id == PLocStation destination