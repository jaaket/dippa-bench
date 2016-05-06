{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Mtl.State where

import           Control.Monad.Reader
import           Control.Monad.State.Strict

import           Mtl.StateTH


genReadersAboveState 10
genReadersBelowState 10
