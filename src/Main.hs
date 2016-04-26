{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Arrow                          ((&&&))
import           Control.Exception                      (bracket)
import           Control.Lens                           ((&), (.~))
import           Control.Monad                          (when)
import           Criterion
import           Criterion.Types                        (Report)
import qualified Data.Binary                            as Bin
import qualified Data.ByteString.Lazy                   as B
import           Data.Default                           (def)
import           Data.Function                          (on)
import           Data.List                              (nub, sort, sortBy)
import           Data.Maybe                             (fromMaybe)
import           Data.Monoid                            ((<>))
import qualified Data.Text                              as T
import           GHC.Generics                           (Generic)
import           Graphics.Rendering.Chart.Backend.Cairo (FileFormat (..),
                                                         fo_format,
                                                         renderableToFile)
import qualified Options.Generic                        as Opts
import           System.Directory                       (doesFileExist)
import           System.IO                              (BufferMode (..),
                                                         hGetBuffering,
                                                         hSetBuffering, stdin)

import           Bench
import qualified Classes.Countdown                      as Classes
import qualified Classes.Exception                      as Classes
import qualified Classes.Reader                         as Classes
import qualified Classes.State                          as Classes
import qualified Classes.StateWriter                    as Classes
import qualified Classes.Writer                         as Classes
import qualified Effects.Countdown                      as Effects
import qualified Effects.NQueens                        as Effects
import qualified Extensible.Countdown                   as Extensible
import qualified Extensible.Exception                   as Extensible
import qualified Extensible.NQueens                     as Extensible
import qualified Extensible.Reader                      as Extensible
import qualified Extensible.State                       as Extensible
import qualified Extensible.StateWriter                 as Extensible
import qualified Extensible.Writer                      as Extensible
import qualified Freer.Countdown                        as Freer
import qualified Freer.Exception                        as Freer
import qualified Freer.NQueens                          as Freer
import qualified Freer.Reader                           as Freer
import qualified Freer.State                            as Freer
import qualified Freer.StateWriter                      as Freer
import qualified Freer.Writer                           as Freer
import qualified Mtl.Countdown                          as Mtl
import qualified Mtl.Exception                          as Mtl
import qualified Mtl.NQueens                            as Mtl
import qualified Mtl.Reader                             as Mtl
import qualified Mtl.State                              as Mtl
import qualified Mtl.StateWriter                        as Mtl
import qualified Mtl.Writer                             as Mtl
import           Plot


steps :: Num a => a -> a -> Int -> [a]
steps _ _ 0 = []
steps low stepSize n = low : steps (low + stepSize) stepSize (n - 1)

benchmarks = [
      let numIters = steps (10^6) (10^6) 5 in
      BenchGroup {
            bgId = "cd"
          , bgDescription = "countdown"
          , bgBenches = map (\(name, benchmark) ->
                  Bench name (map (fromIntegral &&& whnf benchmark) numIters)
                )
                [ ("monad-classes", Classes.countdown)
                , ("effects", Effects.countdown)
                , ("extensible-effects", Extensible.countdown)
                , ("freer", Freer.countdown)
                , ("mtl", Mtl.countdown)]
          , bgXAxisName = "# of iterations"
      }

    , BenchGroup {
        bgId = "ras"
      , bgDescription = "readers above state"
      , bgBenches = (\n ->
          [
            Bench "mtl" (zip [1..5] [
                  whnf Mtl.readersAboveState1 n
                , whnf Mtl.readersAboveState2 n
                , whnf Mtl.readersAboveState3 n
                , whnf Mtl.readersAboveState4 n
                , whnf Mtl.readersAboveState5 n
                ])
          , Bench "freer" (zip [1..5] [
                whnf Freer.readersAboveState1 n
              , whnf Freer.readersAboveState2 n
              , whnf Freer.readersAboveState3 n
              , whnf Freer.readersAboveState4 n
              , whnf Freer.readersAboveState5 n
              ])
          , Bench "extensible-effects" (zip [1..5] [
                whnf Extensible.readersAboveState1 n
              , whnf Extensible.readersAboveState2 n
              , whnf Extensible.readersAboveState3 n
              , whnf Extensible.readersAboveState4 n
              , whnf Extensible.readersAboveState5 n
              ])
          , Bench "monad-classes" (zip [1..5] [
                whnf Classes.readersAboveState1 n
              , whnf Classes.readersAboveState2 n
              , whnf Classes.readersAboveState3 n
              , whnf Classes.readersAboveState4 n
              , whnf Classes.readersAboveState5 n
              ])
          ]) (10^6)
      , bgXAxisName = "# of Reader layers above State"
      }

    , BenchGroup {
          bgId = "rbs"
        , bgDescription = "readers below state"
        , bgBenches = (\n ->
            [
              Bench "mtl" (zip [1..5] [
                    whnf Mtl.readersBelowState1 n
                  , whnf Mtl.readersBelowState2 n
                  , whnf Mtl.readersBelowState3 n
                  , whnf Mtl.readersBelowState4 n
                  , whnf Mtl.readersBelowState5 n
                  ])
            , Bench "freer" (zip [1..5] [
                  whnf Freer.readersBelowState1 n
                , whnf Freer.readersBelowState2 n
                , whnf Freer.readersBelowState3 n
                , whnf Freer.readersBelowState4 n
                , whnf Freer.readersBelowState5 n
                ])
            , Bench "extensible-effects" (zip [1..5] [
                  whnf Extensible.readersBelowState1 n
                , whnf Extensible.readersBelowState2 n
                , whnf Extensible.readersBelowState3 n
                , whnf Extensible.readersBelowState4 n
                , whnf Extensible.readersBelowState5 n
                ])
            , Bench "monad-classes" (zip [1..5] [
                  whnf Classes.readersBelowState1 n
                , whnf Classes.readersBelowState2 n
                , whnf Classes.readersBelowState3 n
                , whnf Classes.readersBelowState4 n
                , whnf Classes.readersBelowState5 n
                ])
            ]) (10^6)
        , bgXAxisName = "# of Reader layers below State"
        }

    , let numIters = steps (10^6) (10^6) 5 in
      BenchGroup {
          bgId = "exc"
        , bgDescription = "exception"
        , bgBenches = map (\(name, benchmark) ->
                Bench name (map (fromIntegral &&& whnf benchmark) numIters)
              )
              [ ("monad-classes", Classes.exception)
              , ("extensible-effects", Extensible.exception)
              , ("freer", Freer.exception)
              , ("mtl", Mtl.exception)]
        , bgXAxisName = "# of iterations"
        }

    , BenchGroup {
          bgId = "nq"
        , bgDescription = "n-queens"
        , bgBenches = map (\(name, benchmark) ->
                Bench name (map (fromIntegral &&& nf benchmark) [6..10])
              )
              [
                ("effects", Effects.nQueens)
              , ("extensible-effects", Extensible.nQueens)
              , ("freer", Freer.nQueens)
              , ("mtl", Mtl.nQueens)
              ]
        , bgXAxisName = "n"
        }

    , let numIters = steps (10^6) (10^6) 5 in
      BenchGroup {
          bgId = "cdr"
        , bgDescription = "countdownReader"
        , bgBenches = map (\(name, benchmark) ->
                Bench name (map (fromIntegral &&& whnf benchmark) numIters)
              )
              [
                ("monad-classes", Classes.countdownReader)
              , ("extensible-effects", Extensible.countdownReader)
              , ("freer", Freer.countdownReader)
              , ("mtl", Mtl.countdownReader)
              ]
        , bgXAxisName = "# of iterations"
        }

    , let numIters = steps (10^5) (10^5) 5 in
      BenchGroup {
          bgId = "rt"
        , bgDescription = "repeatedTell"
        , bgBenches = map (\(name, benchmark) ->
                Bench name (map (fromIntegral &&& nf benchmark) numIters)
              )
              [
                ("monad-classes", Classes.repeatedTell)
              , ("extensible-effects", Extensible.repeatedTell)
              , ("freer", Freer.repeatedTell)
              , ("mtl", Mtl.repeatedTell)
              ]
        , bgXAxisName = "# of iterations"
        }

    , let numIters = steps (10^5) (10^5) 5 in
      BenchGroup {
          bgId = "was"
        , bgDescription = "countdown + writer, writer above state"
        , bgBenches = map (\(name, benchmark) ->
                Bench name (map (fromIntegral &&& nf benchmark) numIters)
              )
              [
                ("monad-classes", Classes.countdownWriterAbove)
              , ("extensible-effects", Extensible.countdownWriterAbove)
              , ("freer", Freer.countdownWriterAbove)
              , ("mtl", Mtl.countdownWriterAbove)
              ]
        , bgXAxisName = "# of iterations"
        }
    , let numIters = steps (10^5) (10^5) 5 in
      BenchGroup {
          bgId = "wbs"
        , bgDescription = "countdown + writer, writer below state"
        , bgBenches = map (\(name, benchmark) ->
                Bench name (map (fromIntegral &&& nf benchmark) numIters)
              )
              [
                ("monad-classes", Classes.countdownWriterBelow)
              , ("extensible-effects", Extensible.countdownWriterBelow)
              , ("freer", Freer.countdownWriterBelow)
              , ("mtl", Mtl.countdownWriterBelow)
              ]
        , bgXAxisName = "# of iterations"
        }
    ]

data Options =
      List
    | Run {
          bench    :: [T.Text]
        , savePath :: Maybe FilePath
        , fw       :: [T.Text]
        }
    | Plot FilePath
    deriving (Generic, Show)

instance Opts.ParseRecord Options

ppBenchGroup :: BenchGroup a -> String
ppBenchGroup group = T.unpack $ bgId group <> ": " <> bgDescription group

aboutToBenchDescription :: [BenchGroup a] -> T.Text
aboutToBenchDescription bgs =
       "I'm about to benchmark:\n"
    <> T.replicate headerIndentLength " "
    <> header <> "\n"
    <> T.intercalate "\n" (map rowText rows)
  where
    headerIndentLength :: Int
    headerIndentLength = maximum (map T.length allFrameworks) + 1

    bgsSortedById = sortBy (compare `on` bgId) bgs

    header :: T.Text
    header = T.concat (map insertSpaceAfter headerItems)
      where
        insertSpaceAfter item =
            item <> T.replicate (headerItemSpacing - T.length item) " "

    headerItems :: [T.Text] -- Benchmark group ids
    headerItems = map bgId bgsSortedById

    headerItemSpacing = headerItemMaxLen + 3

    headerItemMaxLen = maximum (map T.length headerItems)

    rowText :: (T.Text, [Bool]) -> T.Text
    rowText (title, benchPresentList) =
           title <> T.replicate indent " "
        <> T.intercalate (T.replicate (headerItemSpacing - 1) " ") (map displayBool benchPresentList)
      where
        indent = headerIndentLength - T.length title

    rows :: [(T.Text, [Bool])]
    rows = map (id &&& benchesPresent) allFrameworks

    benchesPresent :: T.Text -> [Bool]
    benchesPresent fwName = map (elem fwName . fwsInBench) bgsSortedById
      where
        fwsInBench = map benchDescription . bgBenches

    allFrameworks :: [T.Text]
    allFrameworks =
        sort (nub (map benchDescription (concatMap bgBenches bgs)))

    displayBool :: Bool -> T.Text
    displayBool False = " "
    displayBool True = "✓"

confirmSave :: FilePath -> IO Bool
confirmSave name = bracket
    clearAndStoreBuffering
    (hSetBuffering stdin) $
    \_ -> do
        putStrLn $ "Do you want to overwrite " <> name <> "? [press y to confirm]"
        response <- getChar
        return (response == 'y')
  where
    clearAndStoreBuffering = do
        buf <- hGetBuffering stdin
        hSetBuffering stdin NoBuffering
        return buf

main :: IO ()
main = do
    opts <- Opts.getRecord "Benchmarking" :: IO Options
    case opts of
        List -> do
            putStrLn "Available benchmarks:"
            mapM_ (putStrLn . ppBenchGroup) benchmarks

        Run benches mSavePath fws -> do
            let idsFiltered = if null benches
                    then benchmarks
                    else filter ((`elem` benches) . bgId) benchmarks

            let toBenchmark = if null fws
                    then idsFiltered
                    else map (\bg -> bg { bgBenches = filter ((`elem` fws) . benchDescription) (bgBenches bg) }) idsFiltered

            putStrLn (T.unpack $ aboutToBenchDescription toBenchmark)
            putStrLn ""

            let filename = fromMaybe "results.bin" mSavePath
            destFileExists <- doesFileExist filename
            saveConfirmed <- if destFileExists
                then confirmSave filename
                else return True

            results <- mapM runBenchGroup toBenchmark

            when saveConfirmed (B.writeFile filename (Bin.encode results))

        Plot path -> do
            file <- B.readFile path
            let results = Bin.decode file :: [BenchGroup Report]
            mapM_ (\group -> do
                let filename = T.unpack (bgId group) <> ".pdf"
                putStrLn $ "plot: " <> T.unpack (bgDescription group) <> ": " <> filename
                renderableToFile
                    (def & fo_format .~ PDF)
                    filename
                    (plotBenchGroup group)
                ) results
