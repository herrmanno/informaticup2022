module Main (main) where

import Context (ContextType(..), Context)
import State (fromContext)
import Plan (findBestStateRoute)
import PlanResult (fromState)
import Parser.Parser (parseContext)


main :: IO ()
main = do
    eitherC <- (parseContext @ContextType) <$> getContents
    case eitherC of
        Left err -> print err
        Right c -> run c

run :: Context c => c -> IO ()
run context = do
    let ss = fromContext context
    case findBestStateRoute context ss of
        Just result -> print $ fromState result
        Nothing -> print "ERROR: not best plan found"
