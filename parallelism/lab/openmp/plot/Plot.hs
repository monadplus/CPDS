#!/usr/bin/env nix-shell
#!nix-shell -i runghc

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plot where

import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import Data.Foldable (traverse_)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Vector as Vector
import GHC.Generics
import qualified Graphics.Rendering.Chart.Backend.Diagrams as Backend
import Graphics.Rendering.Chart.Easy
import System.Environment

-------------------------------------------------------

data Data = Data
  { cores :: Int
  , time  :: Double -- ^ Unit: seconds
  } deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Model = Model
  { dat :: [Data]
  } deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

-------------------------------------------------------

printUsage :: IO ()
printUsage = putStrLn "Usage: ./Plot.hs <json file>"

main :: IO ()
main = do
  args <- getArgs
  if (length args == 1)
    then plotFile (args !! 0)
    else printUsage

baseName :: FilePath -> String
baseName = fst . List.span (/= '.')

plotFile :: FilePath -> IO ()
plotFile fp = do
  r <- JSON.eitherDecodeFileStrict' fp
  case r of
    Left err -> error err
    Right model -> do
      printModel (baseName fp ++ ".svg") model

printModel :: FilePath -> Model -> IO ()
printModel fp (Model dat) = toFile fp layout
  where
    layout = layout_title .~ ("Heat Equation - " ++ baseName fp)
           $ layout_margin .~ 20
           $ layout_plots .~ [(toPlot model1)]
           $ layout_x_axis . laxis_title .~ "Cores"
           $ layout_x_axis . laxis_style . axis_label_gap .~ 1
           $ layout_y_axis . laxis_title .~ "Execution Time (seconds)"
           -- $ layout_x_axis . laxis_override .~ axisGridHide
           -- $ layout_y_axis . laxis_override .~ axisGridHide
           $ def

    model1 :: PlotLines Int Double
    model1 = plot_lines_style . line_color .~ opaque red
           $ plot_lines_values .~ [ [ (cores, time) | (Data cores time) <- dat] ]
           $ def


toFile :: (ToRenderable a) => FilePath -> a -> IO ()
toFile fp = void . Backend.renderableToFile def fp . toRenderable
