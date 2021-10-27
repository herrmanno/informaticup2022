module State where

import qualified Data.Map as M
import qualified Data.Set as S
import Types
import Data.Foldable (find)

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

-- stationByID :: State -> ID Station -> Station
-- stationByID s id = let Just station = find ((==id) . s_id) ((S.elems . stations) s) in station

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

type Score = Double

scoreForState :: State -> Score
scoreForState = const 0

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

nextStates :: State -> [State]
nextStates = (:[])

moveTrain :: Train -> State -> [State]
moveTrain train s = case tloc M.! tid of
    TLocConnection c_id s_id distance
        | distance > vel -> [s { trainLocations = M.insert tid (TLocConnection c_id s_id (distance - vel)) tloc }]
        | otherwise -> [s { trainLocations = M.insert tid (TLocStation c_id False) tloc }]
    TLocStation s_id _ -> [s]
        -- TODO: leave train in station (ready to board) <> [send train to all connected connections]
    where
        tid = t_id train
        tloc = trainLocations s
        vel = velocity train

stateIsFinished :: State -> Bool
stateIsFinished State { passengers, passengerLocations } = all isAtDestination passengers  where
    isAtDestination Passenger { p_id, destination } = passengerLocations M.! p_id == PLocStation destination
