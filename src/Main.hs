{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeOperators             #-}

module Main where

import           Control.Arrow                          ((&&&))
import           Control.Exception                      (bracket)
import           Control.Lens                           (to, toListOf, view,
                                                         (%~), (&), (.~), (^.),
                                                         (^..), _2)
import           Control.Monad                          (when)
import           Control.Monad.Par.Class                (NFData)
import           Criterion
import           Criterion.Types                        (Regression (..),
                                                         Report (..),
                                                         SampleAnalysis (..))
import qualified Data.Binary                            as Bin
import qualified Data.ByteString.Lazy                   as B
import qualified Data.Csv                               as Csv
import           Data.Default                           (def)
import           Data.Function                          (on)
import           Data.List                              (nub, sort, sortBy)
import           Data.List.Split                        (splitOn)
import           Data.Maybe                             (fromMaybe)
import           Data.Monoid                            ((<>))
import qualified Data.Text                              as T
import qualified Data.Vector                            as V
import           GHC.Generics                           (Generic)
import           Graphics.Rendering.Chart.Backend.Cairo (FileFormat (..),
                                                         fo_format,
                                                         renderableToFile)
import qualified Options.Generic                        as Opts
import           Statistics.Resampling.Bootstrap        (Estimate (..))
import           System.Directory                       (doesFileExist)
import           System.IO                              (BufferMode (..),
                                                         hGetBuffering,
                                                         hSetBuffering, stdin)

import           Bench
import qualified Classes.Cross                          as Classes
import qualified Classes.Exception                      as Classes
import qualified Classes.Reader                         as Classes
import qualified Classes.RSABS                          as Classes
import qualified Classes.State                          as Classes
import qualified Classes.Writer                         as Classes
import qualified Freer.Cross                            as Freer
import qualified Freer.Exception                        as Freer
import qualified Freer.Reader                           as Freer
import qualified Freer.RSABS                            as Freer
import qualified Freer.State                            as Freer
import qualified Freer.Writer                           as Freer
import           Latex
import qualified Mtl.Cross                              as Mtl
import qualified Mtl.Exception                          as Mtl
import qualified Mtl.Reader                             as Mtl
import qualified Mtl.RSABS                              as Mtl
import qualified Mtl.State                              as Mtl
import qualified Mtl.Writer                             as Mtl
import           Plot


steps :: Num a => a -> a -> Int -> [a]
steps _ _ 0 = []
steps low stepSize n = low : steps (low + stepSize) stepSize (n - 1)

genBenches :: (Integral a, NFData b) => [a] -> [(T.Text, a -> b)] -> [Bench Benchmarkable]
genBenches xValues = map mkBench
  where
    mkBench (name, benchmark) =
        Bench name (map (fromIntegral &&& nf benchmark) xValues)

benchmarks :: [BenchGroup Benchmarkable]
benchmarks = [
      BenchGroup {
            _bgId = "cd"
          , _bgDescription = "State"
          , _bgBenches = genBenches (steps (10^6) (10^6) 10)
                [
                  ("monad-classes", Classes.countdown)
                , ("freer", Freer.countdown)
                , ("mtl", Mtl.countdown)
                ]
          , _bgXAxisName = "# of iterations"
      }

    , BenchGroup {
        _bgId = "ras"
      , _bgDescription = "ReaderStackAboveState"
      , _bgBenches = (\n ks ->
          [
            Bench "freer" (map (\k -> (k, whnf (Freer.readersAboveState k) n)) ks)
          , Bench "monad-classes" (map (\k -> (k, whnf (Classes.readersAboveState k) n)) ks)
          , Bench "mtl" (map (\k -> (k, whnf (Mtl.readersAboveState k) n)) ks)
          ]) (10^6) [0..10]
      , _bgXAxisName = "# of Reader layers above State"
      }

    , BenchGroup {
          _bgId = "rbs"
        , _bgDescription = "ReaderStackBelowState"
        , _bgBenches = (\n ks ->
            [
              Bench "freer" (map (\k -> (k, whnf (Freer.readersBelowState k) n)) ks)
            , Bench "monad-classes" (map (\k -> (k, whnf (Classes.readersBelowState k) n)) ks)
            , Bench "mtl" (map (\k -> (k, whnf (Mtl.readersBelowState k) n)) ks)
            ]) (10^6) [0..10]
        , _bgXAxisName = "# of Reader layers below State"
        }

    , BenchGroup {
          _bgId = "exc"
        , _bgDescription = "Exception"
        , _bgBenches = genBenches (steps (10^8) (10^8) 10)
              [
                ("monad-classes", Classes.exception)
              , ("freer", Freer.exception)
              , ("mtl", Mtl.exception)
              ]
        , _bgXAxisName = "# of iterations"
        }

    , BenchGroup {
          _bgId = "cdr"
        , _bgDescription = "Reader"
        , _bgBenches = genBenches (steps (10^6) (10^6) 10)
              [
                ("monad-classes", Classes.countdownReader)
              , ("freer", Freer.countdownReader)
              , ("mtl", Mtl.countdownReader)
              ]
        , _bgXAxisName = "# of iterations"
        }

    , BenchGroup {
          _bgId = "rt"
        , _bgDescription = "Writer"
        , _bgBenches = genBenches (steps (10^5) (10^5) 10)
              [
                ("monad-classes", Classes.repeatedTell)
              , ("freer", Freer.repeatedTell)
              , ("mtl", Mtl.repeatedTell)
              ]
        , _bgXAxisName = "# of iterations"
        }
    ] ++ map parseDeclaration crossBenchDeclarations

data BenchDecl = forall a b. (Integral a, NFData b) => BenchDecl {
      bdName        :: T.Text
    , bdDescription :: T.Text
    , bdMagnitude   :: a
    , bdBenches     :: [(T.Text, a -> b)]
    }

parseDeclaration :: BenchDecl -> BenchGroup Benchmarkable
parseDeclaration (BenchDecl name desc magnitude fwsAndFuns) = BenchGroup {
      _bgId = name
    , _bgDescription = desc
    , _bgBenches = genBenches (steps magnitude magnitude 10) fwsAndFuns
    , _bgXAxisName = "# of iterations"
    }

crossBenchDeclarations :: [BenchDecl]
crossBenchDeclarations = [
      BenchDecl "ss" "StateAboveState" (10^6) [
          ("freer", Freer.stateState)
        , ("monad-classes", Classes.stateState)
        , ("mtl", Mtl.stateState)
        ]
    , BenchDecl "sr" "StateAboveReader" (10^7) [
          ("freer", Freer.stateReader)
        , ("monad-classes", Classes.stateReader)
        , ("mtl", Mtl.stateReader)
        ]
    , BenchDecl "sw" "StateAboveWriter" (10^6) [
          ("freer", Freer.stateWriter)
        , ("monad-classes", Classes.stateWriter)
        , ("mtl", Mtl.stateWriter)
        ]
    , BenchDecl "se" "StateAboveException" (10^7) [
          ("freer", Freer.stateException)
        , ("monad-classes", Classes.stateException)
        , ("mtl", Mtl.stateException)
        ]
    , BenchDecl "rs" "ReaderAboveState" (10^7) [
          ("freer", Freer.readerState)
        , ("monad-classes", Classes.readerState)
        , ("mtl", Mtl.readerState)
        ]
    , BenchDecl "rr" "ReaderAboveReader" (10^3) [
          ("freer", Freer.readerReader)
        , ("monad-classes", Classes.readerReader)
        , ("mtl", Mtl.readerReader)
        ]
    , BenchDecl "rw" "ReaderAboveWriter" (10^3) [
          ("freer", Freer.readerWriter)
        , ("monad-classes", Classes.readerWriter)
        , ("mtl", Mtl.readerWriter)
        ]
    , BenchDecl "re" "ReaderAboveException" (10^6) [
          ("freer", Freer.readerException)
        , ("monad-classes", Classes.readerException)
        , ("mtl", Mtl.readerException)
        ]
    , BenchDecl "ws" "WriterAboveState" (10^6) [
          ("freer", Freer.writerState)
        , ("monad-classes", Classes.writerState)
        , ("mtl", Mtl.writerState)
        ]
    , BenchDecl "wr" "WriterAboveReader" (10^3) [
          ("freer", Freer.writerReader)
        , ("monad-classes", Classes.writerReader)
        , ("mtl", Mtl.writerReader)
        ]
    , BenchDecl "ww" "WriterAboveWriter" (10^3) [
          ("freer", Freer.writerWriter)
        , ("monad-classes", Classes.writerWriter)
        , ("mtl", Mtl.writerWriter)
        ]
    , BenchDecl "we" "WriterAboveException" (10^6) [
          ("freer", Freer.writerException)
        , ("monad-classes", Classes.writerException)
        , ("mtl", Mtl.writerException)
        ]
    , BenchDecl "es" "ExceptionAboveState" (10^7) [
          ("freer", Freer.exceptionState)
        , ("monad-classes", Classes.exceptionState)
        , ("mtl", Mtl.exceptionState)
        ]
    , BenchDecl "er" "ExceptionAboveReader" (10^6) [
          ("freer", Freer.exceptionReader)
        , ("monad-classes", Classes.exceptionReader)
        , ("mtl", Mtl.exceptionReader)
        ]
    , BenchDecl "ew" "ExceptionAboveWriter" (10^6) [
          ("freer", Freer.exceptionWriter)
        , ("monad-classes", Classes.exceptionWriter)
        , ("mtl", Mtl.exceptionWriter)
        ]
    , BenchDecl "ee" "ExceptionAboveException" (10^8) [
          ("freer", Freer.exceptionException)
        , ("monad-classes", Classes.exceptionException)
        , ("mtl", Mtl.exceptionException)
        ]
    ]

data Options =
      List
    | Run {
          bench    :: [T.Text]
        , savePath :: Maybe FilePath
        , fw       :: [T.Text]
        }
    | Plot FilePath
    | Latex [FilePath]
    | RSquared FilePath
    | Csv FilePath
    | Split FilePath
    | Regressions FilePath
    deriving (Generic, Show)

instance Opts.ParseRecord Options

ppBenchGroup :: BenchGroup a -> String
ppBenchGroup group = T.unpack $ group ^. bgId <> ": " <> group ^. bgDescription

aboutToBenchDescription :: [BenchGroup a] -> T.Text
aboutToBenchDescription bgs =
       "I'm about to benchmark:\n"
    <> T.replicate headerIndentLength " "
    <> header <> "\n"
    <> T.intercalate "\n" (map rowText rows)
  where
    headerIndentLength :: Int
    headerIndentLength = maximum (map T.length allFrameworks) + 1

    bgsSortedById = sortBy (compare `on` view bgId) bgs

    header :: T.Text
    header = T.concat (map insertSpaceAfter headerItems)
      where
        insertSpaceAfter item =
            item <> T.replicate (headerItemSpacing - T.length item) " "

    headerItems :: [T.Text] -- Benchmark group ids
    headerItems = map (view bgId) bgsSortedById

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
        fwsInBench = toListOf (bgBenches . traverse . benchDescription)

    allFrameworks :: [T.Text]
    allFrameworks =
        sort (nub (bgs ^.. traverse . bgBenches . traverse . benchDescription))

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
                    else filter ((`elem` benches) . view bgId) benchmarks

            let toBenchmark = if null fws
                    then idsFiltered
                    else idsFiltered & traverse . bgBenches %~ filter ((`elem` fws) . view benchDescription)

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
            results <- loadAndSortByDescription path
            mapM_ (\group -> do
                let filename = T.unpack (group ^. bgId) <> ".pdf"
                putStrLn $ "plot: " <> T.unpack (group ^. bgDescription) <> ": " <> filename
                renderableToFile
                    (def & fo_format .~ PDF)
                    filename
                    (plotBenchGroup group)
                ) results

        Latex paths -> do
            results <- concat <$> mapM loadAndSortByDescription paths
            mapM_ (putStrLn . T.unpack . exportTable) results

        RSquared path -> do
            results <- loadAndSortByDescription path
            let qualities = concatMap qualityReport results
            putStrLn $ "Worst R^2 value: " <> show (minimum (concatMap qRSquared qualities))
            case filter (not . all (>= 0.99) . qRSquared) qualities of
                [] -> putStrLn "All R^2 are over 0.99"
                qs -> do
                    putStrLn "NOT all R^2 are over 0.99:"
                    mapM_ (\q -> print $ qGroupDescription q <> "/" <> qBenchDescription q) qs

        Csv path -> do
            results <- loadAndSortByDescription path
            mapM_ (\group -> do
                let filename = T.unpack (group ^. bgId) <> ".csv"
                putStrLn $ "export csv: " <> T.unpack (group ^. bgDescription) <> ": " <> filename
                B.writeFile filename (toCsv (fmap getOLS group))
                ) results

        Regressions path -> do
            csv <- B.readFile path
            let Right regressions = fmap V.toList (Csv.decode Csv.HasHeader csv)
            let output = exportRegressions benchmarks regressions
            putStrLn (T.unpack output)

        Split path -> do
            results <- loadAndSortByDescription path
            let nameParts = splitOn ['.'] path
            mapM_ (\group -> do
                let newPath = concat (init nameParts)
                           ++ "."
                           ++ T.unpack (group ^. bgId)
                           ++ "."
                           ++ last nameParts
                B.writeFile newPath (Bin.encode [group])
                ) results

qualityReport :: BenchGroup Report -> [Quality]
qualityReport group = map buildQuality (group ^. bgBenches)
  where
    buildQuality :: Bench Report -> Quality
    buildQuality bench = Quality {
          qBenchDescription = bench ^. benchDescription
        , qGroupDescription = group ^. bgDescription
        , qRSquared = bench ^.. benchData . traverse . _2 . to reportAnalysis .
                                to anRegress . to head . to regRSquare .
                                to estPoint
        }

data Quality = Quality {
      qBenchDescription :: T.Text
    , qGroupDescription :: T.Text
    , qRSquared         :: [Double]
    } deriving Show

loadAndSortByDescription :: FilePath -> IO [BenchGroup Report]
loadAndSortByDescription path = do
    file <- B.readFile path
    let results = Bin.decode file :: [BenchGroup Report]
    return (results & traverse . bgBenches %~ sortBy (compare `on` view benchDescription))
