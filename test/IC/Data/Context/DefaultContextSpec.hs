module IC.Data.Context.DefaultContextSpec (spec) where

import qualified Data.Map                       as M
import qualified Data.Set                       as S
import           IC.Data.Connection             (Connection (Connection))
import           IC.Data.Context.DefaultContext (DefaultContext (_connections, _stations, _trains),
                                                 connectionsFrom, emptyContext,
                                                 setTrainStartPosition,
                                                 setTrainStartPositions)
import           IC.Data.Station                (Station (Station))
import           IC.Data.Train                  (Train (Train),
                                                 TrainAction (Start),
                                                 TrainLocation (TLocStation),
                                                 TrainStatus (Boardable))
import           Test.Hspec                     (SpecWith, describe, it,
                                                 shouldBe)

spec :: SpecWith ()
spec = do
    describe "Context" $ do
        it "should return all connection from / to a given station" $ do
            let c1 = Connection 1 (1,2) 0 0
            let c2 = Connection 2 (2,1) 0 0
            let c3 = Connection 3 (1,3) 0 0
            let ctx = emptyContext {
                _connections = S.fromList [c1, c2, c3]
            }
            connectionsFrom ctx 1 `shouldBe` S.fromList [(c1,2),(c2,2),(c3,3)]
            connectionsFrom ctx 2 `shouldBe` S.fromList [(c1,1),(c2,1)]
            connectionsFrom ctx 3 `shouldBe` S.fromList [(c3,1)]
            connectionsFrom ctx 4 `shouldBe` S.empty

        it "should return static train locations" $ do
            let trainID = 1
            let ctx = emptyContext { _trains = S.singleton (Train trainID (Just 1) 1 1) }
            let result = setTrainStartPosition ctx trainID
            let target =
                    [   ( M.singleton trainID (TLocStation 1 Boardable)
                        , M.empty
                        )
                    ]
            S.fromList result `shouldBe` S.fromList target

        it "should set train start to all available stations" $ do
            let trainID = 1
            let ctx = emptyContext
                    { _trains = S.singleton (Train trainID Nothing 1 1)
                    , _stations = S.fromList [ Station 1 1, Station 2 2]
                    }
            let result = setTrainStartPosition ctx trainID
            let target =
                    [   ( M.singleton trainID (TLocStation 1 Boardable)
                        , M.singleton trainID [Start 0 1]
                        )
                    ,   ( M.singleton trainID (TLocStation 2 Boardable)
                        , M.singleton trainID [Start 0 2]
                        )
                    ]
            S.fromList result `shouldBe` S.fromList target

        it "should set all train starts to all available stations" $ do
            let ctx = emptyContext
                    { _trains = S.fromList [Train 1 Nothing 1 1, Train 2 Nothing 1 1]
                    , _stations = S.fromList [Station 1 1, Station 2 2]
                    }
            let result = setTrainStartPositions ctx
            let target =
                    [   ( M.fromList [(1, TLocStation 1 Boardable), (2, TLocStation 1 Boardable)]
                        , M.fromList [(1, [Start 0 1]), (2, [Start 0 1])]
                        )
                    ,   ( M.fromList [(1, TLocStation 1 Boardable), (2, TLocStation 2 Boardable)]
                        , M.fromList [(1, [Start 0 1]), (2, [Start 0 2])]
                        )
                    ,   ( M.fromList [(1, TLocStation 2 Boardable), (2, TLocStation 1 Boardable)]
                        , M.fromList [(1, [Start 0 2]), (2, [Start 0 1])]
                        )
                    ,   ( M.fromList [(1, TLocStation 2 Boardable), (2, TLocStation 2 Boardable)]
                        , M.fromList [(1, [Start 0 2]), (2, [Start 0 2])]
                        )
                    ]
            S.fromList result `shouldBe` S.fromList target
