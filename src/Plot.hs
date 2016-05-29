{-# LANGUAGE FlexibleContexts  #-}

module Plot where

import Control.Lens
import           Criterion.Types                        (Report (..))
import           Control.Arrow                          (second)
import qualified Control.Monad.State                    as State
import           Data.Function                          (on)
import           Data.List                              (sortBy)
import qualified Data.Text                              as T
import qualified Data.Text.Lazy                         as LT
import           Data.Text.Lazy.Builder                 (toLazyText)
import           Data.Text.Lazy.Builder.RealFloat       (formatRealFloat,
                                                        FPFormat (..))
import qualified Data.Colour.SRGB                       as Colour
import           Graphics.Rendering.Chart.Easy
import           Graphics.Rendering.Chart.Utils (isValidNumber)

import Bench


getPoints :: Bench Report -> [(Double, Double)]
getPoints = map (second getOLS) . view benchData

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
    title = T.unpack (bench ^. benchDescription)

plotBenchGroup group = toRenderable layout
  where
    sortedBenches = sortBy (compare `on` view benchDescription) (group ^. bgBenches)

    plots = State.evalState (mapM plotBench sortedBenches) (newCycle styles)

    xAxisName = T.unpack (group ^. bgXAxisName)

    layout = layout_plots .~ map toPlot plots
           $ layout_x_axis . laxis_title .~ xAxisName
           $ layout_x_axis . laxis_generate .~ autoScaledPaddedAxis 0.1 def
           $ layout_y_axis . laxis_title .~ "time (s)"
           $ layout_all_font_styles %~ ((font_size .~ 40) . (font_name .~ "Gill Sans"))
           $ def

    styleCommon = point_radius .~ 10
                $ point_color .~ transparent
                $ point_border_width .~ 3
                $ def

    styles = map ($ styleCommon) [
          (point_border_color .~ opaque (Colour.sRGB24read "e41a1c")) .
          (point_shape .~ PointShapeCross)
        , (point_border_color .~ opaque (Colour.sRGB24read "377eb8")) .
          (point_shape .~ PointShapePlus)
        , (point_border_color .~ opaque (Colour.sRGB24read "4daf4a")) .
          (point_shape .~ PointShapeCircle)
        , (point_border_color .~ opaque (Colour.sRGB24read "984ea3")) .
          (point_shape .~ PointShapePolygon 3 False)
        , (point_border_color .~ opaque (Colour.sRGB24read "ff7f00")) .
          (point_shape .~ PointShapePolygon 3 True)
        ]

    autoScaledPaddedAxis :: RealFloat a => a -> LinearAxisParams a -> AxisFn a
    autoScaledPaddedAxis padding lap ps0 = scaledAxis lapShorterLabels rs ps
      where
        ps = filter isValidNumber ps0
        width = maximum ps - minimum ps
        rs = (minimum ps - padding * width, maximum ps + padding * width)
        lapShorterLabels = lap & la_labelf .~
            LT.unpack . toLazyText . formatRealFloat Exponent (Just 1)
