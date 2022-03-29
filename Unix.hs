{-# LANGUAGE RecordWildCards #-}

module Unix(build, install, bootquick) where

import System.Info.Extra
import Development.Rattle
import qualified System.Directory as D
import System.FilePath
import Control.Monad.Extra
import qualified Config as C
import qualified S.Unix as S.Unix
import qualified S.Base as S.Base
import qualified S.Config as S
import qualified C.Unix as C.Unix
import C.Base (pbchunksrc)
import Mach
import Configure
import S.Cross
import qualified Install as I

bootTarget :: FilePath -> Mach -> Integer -> Integer -> String -> C.Config -> Run ()
bootTarget cd m' o d what C.Config{..} = do
  cmd [Cwd $ cd </> ".."] [srcdir </> "workarea", showMach m', "xc-" ++ showMach m', showMach m]
  liftIO $ C.Unix.linkeach (cd </> ".." </> "xc-" ++ showMach m' </> "s") (upupsrcdir </> "s")
  c <- liftIO $ semiConfigure cd m'
  xboot (cd </> ".." </> "xc-" ++ showMach m' </> "s") ("../.." </> showMach m) what m o d c S.Config{..}
  -- cmd [Cwd $ ".." </> "xc-" ++ showMach m' </> "s"] ["make", "-f", "Mf-cross", "base=../../pb", "m=pb", "xm=ta6le"]

  S.Base.keepbootfiles (cd </> ".." </> "xc-" ++ showMach m' </> "s") ("../boot/tmp" </> showMach m') c

bootquick :: FilePath -> Mach -> C.Config -> Int -> Run ()
bootquick cd m' config@C.Config{..} j = do
  liftIO $ C.Unix.linkeach (cd </> "c") (C.upupsrcdir config </> "c")
  pbs <- liftIO $ pbchunksrc cd $ showMach $ C.m config
  liftIO $ C.Unix.linkList (cd </> "c") ("../boot" </> showMach m) pbs
  C.Unix.build (cd </> "c") config j
  -- $(MAKE) -f Mf-boot $*.boot o=3 d=0 what=all
  bootTarget cd m' 3 0 "all" config
  

build :: FilePath -> C.Config -> Int -> Run ()
build cd config@C.Config{..} j = do
  -- cd c && make
  liftIO $ C.Unix.linkeach (cd </> "c") (C.upupsrcdir config </> "c")
  pbs <- liftIO $ pbchunksrc cd $ showMach $ C.m config
  liftIO $ C.Unix.linkList (cd </> "c") ("../boot" </> showMach m) $ pbs
  C.Unix.build (cd </> "c") config j
  -- cd s && make bootstrap
  liftIO $ C.Unix.linkeach (cd </> "s") (C.upupsrcdir config </> "s")
  S.Unix.build (cd </> "s") config

installKernelTarget = "installkernellib"

install :: FilePath -> String -> FilePath -> C.Config -> Run ()
install = I.install
