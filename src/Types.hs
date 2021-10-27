module Types where

type Time = Int

type ID a = Int

data Station = Station
    { s_id :: ID Station
    , s_capacity :: Int
    } deriving (Eq, Show)

data Connection = Connection
    { c_id :: ID Connection
    , c_stations :: (ID Station, ID Station)
    , c_capacity :: Int
    , distance :: Double
    } deriving (Eq, Show)

data Train = Train
    { t_id :: ID Train
    , start :: Maybe Station
    , velocity :: Double
    , t_capacity :: Int
    } deriving (Eq, Show)

data TrainLocation = TLocStation
                        (ID Station)    -- ^ current station
                        Bool            -- ^ if the train is ready to board
                   | TLocConnection
                        (ID Connection) -- ^ current location
                        (ID Station)    -- ^ station heading to
                        Double          -- ^ remaining distance to station
                   deriving (Eq)

data TrainAction = Start (ID Station)
                 | Depart (ID Connection) 
                 deriving (Eq)

data Passenger = Passenger
    { p_id :: ID Passenger
    , departure :: ID Station
    , destination :: ID Station
    , size :: Int
    , arrival :: Int
    } deriving (Eq, Show)

data PassengerLocation = PLocStation (ID Station)
                       | PLocTrain (ID Train)
                       deriving (Eq)

data PassengerAction = Board (ID Train)
                     | Detrain
                     deriving (Eq)