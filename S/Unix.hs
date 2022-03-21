{-# LANGUAGE RecordWildCards #-}

module S.Unix(build) where

import System.Directory
import Config
import qualified S.Base as B
import qualified S.Config as S
import Development.Rattle

build :: Config -> Run ()
build Config{..} = B.bootstrap (S.defaultVars m) S.Config{..}
