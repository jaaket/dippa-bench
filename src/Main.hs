{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Criterion
import           Criterion.Types
import qualified Data.Binary          as Bin
import qualified Data.ByteString.Lazy as B
import           Options.Generic

import           Mtl


data Options =
      Run { save :: Maybe FilePath <?> "Save results to this file" }
    | Load FilePath
    deriving (Generic, Show)

instance ParseRecord Options

runBenchmarks :: IO Report
runBenchmarks = benchmark' (whnf countdown 1000)

readResult :: FilePath -> IO Report
readResult path = do
    file <- B.readFile path
    return (Bin.decode file)

main :: IO ()
main = do
    opts <- getRecord "Effect benchmarks"
    report <- case opts of
        Run mSavePath -> do
            report <- runBenchmarks
            case unHelpful mSavePath of
                Just path -> B.writeFile path (Bin.encode report)
                Nothing -> return ()
            return report
        Load path -> readResult path
    print (anMean $ reportAnalysis report)
