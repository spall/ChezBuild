{-# LANGUAGE RecordWildCards #-}

module S.Unix(build) where

import System.Directory
import Config
import qualified S.Base as B
import qualified S.Config as S
import Development.Rattle

build :: FilePath -> Config -> Run ()
build cd Config{..} = B.bootstrap cd (S.defaultVars m) S.Config{..}
