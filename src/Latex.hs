module Latex where

import           Criterion.Types        (Report (..))
import           Data.List              (intersperse, transpose)
import qualified Data.Text              as T
import           Text.LaTeX
import           Text.LaTeX.Base.Class
import           Text.LaTeX.Base.Syntax

import           Bench


exportTable :: BenchGroup Report -> T.Text
exportTable = render . resultsToLatex

resultsToLatex :: BenchGroup Report -> LaTeX
resultsToLatex group = tabular Nothing tableSpec table
  where
    tableSpec = [CenterColumn, VerticalLine] ++ replicate numBenches CenterColumn

    numBenches = length (bgBenches group)

    table = toprule <> header <> lnbk <> midrule <> body <> lnbk <> bottomrule

    header = fromString (T.unpack $ bgXAxisName group)
           & foldr1 (&) (map (fromString . T.unpack . benchDescription) (bgBenches group))

    body = foldr1 (<>) (intersperse lnbk (map rowToLatex rows))

    rowToLatex :: [Double] -> LaTeX
    rowToLatex (hd:tl) = foldr1 (&) $
        fromString (show hd) : map (fromString . show) tl

    rows = transpose (xValues : map (map snd . benchData . fmap getMean) (bgBenches group))

    xValues = map fst (benchData (head (bgBenches group)))

toprule :: LaTeXC l => l
toprule = fromLaTeX (TeXCommS "toprule")

midrule :: LaTeXC l => l
midrule = fromLaTeX (TeXCommS "midrule")

bottomrule :: LaTeXC l => l
bottomrule = fromLaTeX (TeXCommS "bottomrule")
