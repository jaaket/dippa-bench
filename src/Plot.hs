{-# LANGUAGE FlexibleContexts  #-}

module Plot where

import           Criterion.Types                        (Report (..),
                                                         SampleAnalysis (..))
import           Statistics.Resampling.Bootstrap        (Estimate (..))
import           Control.Arrow                          (second)
import qualified Control.Monad.State                    as State
import qualified Data.Text                              as T
import qualified Data.Colour.SRGB                       as Colour
import           Graphics.Rendering.Chart.Easy

import Bench


getMean :: Report -> Double
getMean = estPoint . anMean . reportAnalysis

getPoints :: Bench Report -> [(Double, Double)]
getPoints = map (second getMean) . benchData

data Cycle a = Cycle { cycleValues :: [a], cycleCurrent :: Int }

newCycle :: [a] -> Cycle a
newCycle values = Cycle values 0

getCycleValue :: State.MonadState (Cycle a) m => m a
getCycleValue = do
    cycle <- State.get
    let next = cycleCurrent cycle + 1 `mod` length (cycleValues cycle)
    State.put (cycle { cycleCurrent = next })
    return (cycleValues cycle !! cycleCurrent cycle)

plotBench bench = do
    style <- getCycleValue
    return $ plot_points_values .~ getPoints bench
           $ plot_points_title .~ title
           $ plot_points_style .~ style
           $ def
  where
    title = T.unpack (benchDescription bench)

plotBenchGroup group = toRenderable layout
  where
    plots = State.evalState (mapM plotBench (bgBenches group)) (newCycle styles)

    xAxisName = T.unpack (bgXAxisName group)

    layout = layout_title .~ T.unpack (bgDescription group)
           $ layout_plots .~ map toPlot plots
           $ layout_x_axis . laxis_title .~ xAxisName
           $ layout_y_axis . laxis_title .~ "time (s)"
           $ def

    styles = map ($ point_radius .~ 5 $ def) [
          (point_color .~ opaque (Colour.sRGB24read "e41a1c")) .
          (point_shape .~ PointShapeCircle)
        , (point_color .~ opaque (Colour.sRGB24read "377eb8")) .
          (point_shape .~ PointShapePolygon 4 False)
        , (point_color .~ opaque (Colour.sRGB24read "4daf4a")) .
          (point_shape .~ PointShapePolygon 4 True)
        , (point_color .~ opaque (Colour.sRGB24read "984ea3")) .
          (point_shape .~ PointShapePolygon 3 False)
        , (point_color .~ opaque (Colour.sRGB24read "ff7f00")) .
          (point_shape .~ PointShapePolygon 3 True)
        ]
