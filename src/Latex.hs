{-# LANGUAGE OverloadedStrings #-}

module Latex where

import           Control.Lens                (ix, view, (^.), (^?!))
import           Criterion.Types             (Report (..))
import           Data.List                   (find, intercalate, intersperse,
                                              transpose)
import           Data.List.Split             (chunksOf)
import qualified Data.Text                   as T
import qualified Data.Text.Format            as Format
import qualified Data.Text.Lazy              as LT
import qualified Data.Text.Lazy.Builder      as T
import           Text.LaTeX
import           Text.LaTeX.Base.Class
import           Text.LaTeX.Base.Syntax
import           Text.LaTeX.Packages.AMSMath

import           Bench
import           Regression


exportTable :: BenchGroup Report -> T.Text
exportTable = render . resultsToLatex

resultsToLatex :: BenchGroup Report -> LaTeX
resultsToLatex group =
    tableEnv $
           centering
        <> tableCaption
        <> tableLabel
        <> tabular Nothing tableSpec table
  where
    tableSpec = [CenterColumn, VerticalLine] ++ replicate numBenches LeftColumn

    numBenches = length (group ^. bgBenches)

    tableLabel = label (fromString ("table:" ++ T.unpack (group ^. bgId)))

    tableCaption = caption (fromString (T.unpack (group ^. bgDescription)))

    table = toprule <> newline <> header <> lnbk <> midrule <> newline <> body <> lnbk <> bottomrule

    header = fromString (T.unpack $ group ^. bgXAxisName)
           & foldr1 (&) (map (fromString . T.unpack . view benchDescription) (group ^. bgBenches))

    body = foldr1 (<>) (intersperse lnbk (map rowToLatex rows))

    rowToLatex :: [Double] -> LaTeX
    rowToLatex (hd:tl) = foldr1 (&) $
        fromString (show hd) : map (realToLatexWithPrec 3) tl

    rows = transpose (xValues : map (map snd . view benchData . fmap getOLS) (group ^. bgBenches))

    xValues = fmap fst $ group ^?! bgBenches . ix 0 . benchData

exportRegressions :: [BenchGroup a] -> [Regression] -> T.Text
exportRegressions benches = render . regressionsToLatex benches

regressionsToLatex :: [BenchGroup a] -> [Regression] -> LaTeX
regressionsToLatex benches regressions = longtable tableSpec table
  where
    tableSpec = replicate 3 CenterColumn

    table =
           tableCaption <> tableLabel <> lnbk
        <> toprule <> newline
        <> header <> lnbk <> endfirsthead
        <> continuationHeader <> lnbk <> midrule <> endhead
        <> midrule <> newline
        <> body <> lnbk
        <> bottomrule

    tableCaption = caption (fromString "Results of regression")

    tableLabel = label (fromString "table:regression")

    header = fromString "Benchmark" & fromString "Framework" & fromString "Leading term"

    continuationHeader = multicolumn 3 [CenterColumn]
        (textbf $
               tablename
            <> raw "\\ "
            <> thetable
            <> fromString " -- continued from previous page") <> lnbk <> header

    body = foldr1 (<>) (intercalate [midrule, newline]
        (chunksOf 6 (intersperse lnbk (map regToLatex regressions))))

    regToLatex :: Regression -> LaTeX
    regToLatex reg =
          fromString (T.unpack (findDescription benches reg))
        & fromString (T.unpack (regFw reg))
        & math (realToLatexWithPrec 2 (regCoeff reg) <> degreeToLatex (regDegree reg))

    degreeToLatex :: Int -> LaTeX
    degreeToLatex 1 = fromString "n"
    degreeToLatex n = fromString "n" ^: fromString (show n)

    findDescription :: [BenchGroup a] -> Regression -> T.Text
    findDescription benches reg =
        let Just group = find ((== regBench reg) . view bgId) benches
         in group ^. bgDescription

realToLatexWithPrec :: Real a => Int -> a -> LaTeX
realToLatexWithPrec prec =
    fromString . T.unpack . LT.toStrict . T.toLazyText . Format.prec prec

toprule :: LaTeXC l => l
toprule = fromLaTeX (TeXCommS "toprule")

midrule :: LaTeXC l => l
midrule = fromLaTeX (TeXCommS "midrule")

bottomrule :: LaTeXC l => l
bottomrule = fromLaTeX (TeXCommS "bottomrule")

centering :: LaTeXC l => l
centering = fromLaTeX (TeXCommS "centering")

tableEnv :: LaTeXC l => l -> l
tableEnv = liftL (TeXEnv "table" [OptArg (fromString "H")])

longtable :: LaTeXC l => [TableSpec] -> l -> l
longtable ts = liftL (TeXEnv "longtable" [FixArg $ TeXRaw $ renderAppend ts])

endfirsthead :: LaTeXC l => l
endfirsthead = fromLaTeX (TeXCommS "endfirsthead") <> raw "\n"

endhead :: LaTeXC l => l
endhead = fromLaTeX (TeXCommS "endhead") <> raw "\n"

tablename :: LaTeXC l => l
tablename = fromLaTeX (TeXCommS "tablename")

thetable :: LaTeXC l => l
thetable = comm0 "thetable"
