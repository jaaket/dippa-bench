module Latex where

import           Control.Lens           (ix, view, (^.), (^?!))
import           Criterion.Types        (Report (..))
import           Data.List              (intersperse, transpose)
import qualified Data.Text              as T
import qualified Data.Text.Format       as Format
import qualified Data.Text.Lazy         as LT
import qualified Data.Text.Lazy.Builder as T
import           Text.LaTeX
import           Text.LaTeX.Base.Class
import           Text.LaTeX.Base.Syntax

import           Bench


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

    table = toprule <> header <> lnbk <> midrule <> body <> lnbk <> bottomrule

    header = fromString (T.unpack $ group ^. bgXAxisName)
           & foldr1 (&) (map (fromString . T.unpack . view benchDescription) (group ^. bgBenches))

    body = foldr1 (<>) (intersperse lnbk (map rowToLatex rows))

    rowToLatex :: [Double] -> LaTeX
    rowToLatex (hd:tl) = foldr1 (&) $
        fromString (show hd) : map (fromString . T.unpack . LT.toStrict . T.toLazyText . Format.prec 3) tl

    rows = transpose (xValues : map (map snd . view benchData . fmap getOLS) (group ^. bgBenches))

    xValues = fmap fst $ group ^?! bgBenches . ix 0 . benchData

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
