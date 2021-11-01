module State where

import qualified Data.Map as M
import qualified Data.Set as S
import App (App )
import Context (Context(..), connectionsFrom)
import Types (ID)
import Types.Station (Station(..))
import Types.Connection (Connection(..))
import Types.Train (Train(..), TrainLocation(..), TrainAction(..), isBoardable)
import Types.Passenger (Passenger(..), PassengerLocation(..), PassengerAction(..))
import Control.Arrow (second)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, get)

-----------------------------------------------------------
--                  DATA
-----------------------------------------------------------
data State = State
    { time :: Int
    , trainLocations :: M.Map (ID Train) TrainLocation
    , passengerLocations :: M.Map (ID Passenger) PassengerLocation
    , trainActions :: M.Map (ID Train) [TrainAction]
    , passengerActions :: M.Map (ID Passenger) [PassengerAction]
    } deriving (Show, Eq, Ord)

emptyState = State 0 M.empty M.empty M.empty M.empty

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

stateIsFinished :: Monad m => State -> App m Bool
stateIsFinished s = do
    ps <- passengers <$> get
    return $ all isAtDestination ps
    where
        ploc = passengerLocations s
        isAtDestination Passenger { p_id, destination } = ploc M.! p_id == PLocStation destination