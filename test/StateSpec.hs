module StateSpec (spec) where

import Test.Hspec
import Types
import State
import qualified Data.Set as S
import qualified Data.Map as M

-- ctx = Context
--     { stations = S.fromList [ Station 1 1, Station 2 2]
--     , connections = S.fromList [ Connection 1 (1,2) 2 10 ]
--     , trains = S.fromList [ Train 1 (Just 1) 1 1, Train 2 Nothing 2 2, Train 3 Nothing 3 3]
--     , passengers = S.fromList [ Passenger 1 1 2 1 10, Passenger 2 2 1 5 10]
--     }

spec = do
    describe "State" $ do
        it "should set train start to all available stations" $ do
            let trainID = 1
            let ctx = emptyContext
                    { trains = S.singleton (Train trainID Nothing 1 1)
                    , stations = S.fromList [ Station 1 1, Station 2 2]
                    }
            let result = setTrainStartPosition ctx trainID
            let target =
                    [   ( ctx { trains = S.singleton (Train trainID (Just 1) 1 1) }
                        , M.singleton trainID [Start 1]
                        )
                    ,   ( ctx { trains = S.singleton (Train trainID (Just 2) 1 1) }
                        , M.singleton trainID [Start 2]
                        )
                    ]
            S.fromList result `shouldBe` S.fromList target

        it "should set all train starts to all available stations" $ do
            let ctx = emptyContext
                    { trains = S.fromList [Train 1 Nothing 1 1, Train 2 Nothing 1 1]
                    , stations = S.fromList [Station 1 1, Station 2 2]
                    }
            let result = setTrainStartPositions ctx
            let target =
                    [   ( ctx { trains = S.fromList [Train 1 (Just 1) 1 1, Train 2 (Just 1) 1 1] }
                        , M.fromList [(1, [Start 1]), (2, [Start 1])]
                        )
                    ,   ( ctx { trains = S.fromList [Train 1 (Just 1) 1 1, Train 2 (Just 2) 1 1] }
                        , M.fromList [(1, [Start 1]), (2, [Start 2])]
                        )
                    ,   ( ctx { trains = S.fromList [Train 1 (Just 2) 1 1, Train 2 (Just 1) 1 1] }
                        , M.fromList [(1, [Start 2]), (2, [Start 1])]
                        )
                    ,   ( ctx { trains = S.fromList [Train 1 (Just 2) 1 1, Train 2 (Just 2) 1 1] }
                        , M.fromList [(1, [Start 2]), (2, [Start 2])]
                        )
                    ]
            S.fromList result `shouldBe` S.fromList target