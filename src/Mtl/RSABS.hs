{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Mtl.RSABS where

import           Control.Monad.Reader
import           Control.Monad.State.Strict

import           Mtl.RSABSTH


genReadersAboveState 10
genReadersBelowState 10
