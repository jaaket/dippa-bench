{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Freer.State where

import           Freer.StateTH


genReadersAboveState 10
genReadersBelowState 10
