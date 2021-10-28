module State where

import qualified Data.Map as M
import qualified Data.Set as S
import Types

-----------------------------------------------------------
--                  DATA
-----------------------------------------------------------
data State = State
    -- static data
    { stations :: S.Set Station
    , connections :: S.Set Connection
    , trains :: S.Set Train
    , passengers :: S.Set Passenger
    -- dynamic state
    , time :: Time
    , trainLocations :: M.Map (ID Train) TrainLocation
    , passengerLocations :: M.Map (ID Passenger) PassengerLocation
    , trainActions :: M.Map (ID Train) [TrainAction]
    , passengerActions :: M.Map (ID Passenger) [PassengerAction]
    }

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

connectionsFrom :: State -> ID Station -> S.Set (Connection, ID Station)
connectionsFrom s s_id = cs where
    cs = S.map withOtherStation $ S.filter endsAtStation (connections s)
    withOtherStation c@Connection { c_stations = (a,b) }
        | a == s_id = (c, b)
        | otherwise = (c, a)
    endsAtStation c = let (a,b) = c_stations c in a == s_id || b == s_id

-----------------------------------------------------------
--                  Prepare Input
-----------------------------------------------------------

-- | Assigns stations to all pending trains and returns the list of resulting states
setTrainStartPositions :: State -> [State]
setTrainStartPositions s = filter stateIsValid (go s pendingTrainIDs) where
    pendingTrainIDs = S.elems $ S.map t_id $ S.filter hasNoStation (trains s)
    go :: State -> [ID Train] -> [State]
    go _ [] = []
    go s (t:ts) = let ss = setTrainStartPosition s t in concatMap (`go` ts) ss

-- | Returns a list of state where the start station of a train is set to all available stations
setTrainStartPosition :: State -> ID Train -> [State]
setTrainStartPosition s tid =
    [ s { trains = modifyTrain tid (\t -> t { start = Just (s_id station) }) (trains s)
        , trainActions = M.insert tid [Start (s_id station)] (trainActions s)
        }
    | station <- S.elems (stations s)
    ] where
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

stateIsValid :: State -> Bool
stateIsValid s = all stationIsValid (stations s) && all connectionIsValid (connections s) where
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

nextStates :: State -> [State]
nextStates s = let ss = moveTrains ts s in concatMap (movePassengers ps) ss where
    ts = S.elems (trains s)
    ps = S.elems (passengers s)

moveTrains :: [Train] -> State -> [State]
moveTrains [] s = []
moveTrains (t:ts) s = let ss = moveTrain t s in concatMap (moveTrains ts) ss

moveTrain :: Train -> State -> [State]
moveTrain train s = case tloc M.! tid of
    TLocConnection c_id s_id distance
        -- train is on connection in next round
        | distance > vel -> [s { trainLocations = M.insert tid (TLocConnection c_id s_id (distance - vel)) tloc }]
        -- train reaches next station at next round
        | otherwise -> [s { trainLocations = M.insert tid (TLocStation s_id False) tloc }]
    TLocStation s_id _ ->
        let stayingTrain = s { trainLocations = M.insert tid (TLocStation s_id True) tloc }
            connections = connectionsFrom s s_id
            leavingTrains = flip fmap (S.elems connections) $
                \(c, s_id') -> if distance c > vel
                    -- train is on connection in next round
                    then s { trainLocations = M.insert tid (TLocConnection (c_id c) s_id' vel) tloc }
                    -- train reaches next station at next round
                    else s { trainLocations = M.insert tid (TLocStation s_id' False) tloc }
        in stayingTrain : leavingTrains
    where
        tid = t_id train
        tloc = trainLocations s
        vel = velocity train

movePassengers :: [Passenger] -> State -> [State]
movePassengers [] s = []
movePassengers (p:ps) s = let ss = movePassenger p s in concatMap (movePassengers ps) ss

movePassenger :: Passenger -> State -> [State]
movePassenger pas s = case ploc M.! pid of
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

stateIsFinished :: State -> Bool
stateIsFinished State { passengers, passengerLocations } = all isAtDestination passengers  where
    isAtDestination Passenger { p_id, destination } = passengerLocations M.! p_id == PLocStation destination
