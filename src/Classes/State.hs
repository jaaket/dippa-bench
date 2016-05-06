{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Classes.State where

import           Classes.StateTH

genReadersAboveState 10
genReadersBelowState 10
