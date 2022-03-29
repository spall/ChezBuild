{-# LANGUAGE RecordWildCards #-}

module Unix(build, install, bootquick) where

import qualified System.Directory as D
import qualified UnliftIO.Directory as U
import System.Info.Extra
import Development.Rattle
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

bootTarget :: Mach -> Integer -> Integer -> String -> C.Config -> Run ()
bootTarget m' o d what C.Config{..} = do
  cmd [Cwd ".."] [srcdir </> "workarea", showMach m', "xc-" ++ showMach m', showMach m]
  liftIO $ D.withCurrentDirectory (".." </> "xc-" ++ showMach m' </> "s") $ C.Unix.linkeach (upupsrcdir </> "s")
  c <- semiConfigure m'
  U.withCurrentDirectory (".." </> "xc-" ++ showMach m' </> "s") $ xboot ("../.." </> showMach m) what m o d c S.Config{..}
  -- cmd [Cwd $ ".." </> "xc-" ++ showMach m' </> "s"] ["make", "-f", "Mf-cross", "base=../../pb", "m=pb", "xm=ta6le"]

  U.withCurrentDirectory (".." </> "xc-" ++ showMach m' </> "s") $ S.Base.keepbootfiles ("../boot/tmp" </> showMach m') c

bootquick :: Mach -> C.Config -> Int -> Run ()
bootquick m' config@C.Config{..} j = do
  liftIO $ D.withCurrentDirectory "c" $ C.Unix.linkeach (C.upupsrcdir config </> "c")
  pbs <- liftIO $ pbchunksrc $ showMach $ C.m config
  liftIO $ D.withCurrentDirectory "c" $ C.Unix.linkList ("../boot" </> showMach m) pbs
  U.withCurrentDirectory "c" $ C.Unix.build config j
  -- $(MAKE) -f Mf-boot $*.boot o=3 d=0 what=all
  liftIO $ putStrLn "building boottarget"
  bootTarget m' 3 0 "all" config
  

build :: C.Config -> Int -> Run ()
build config@C.Config{..} j = do
  -- cd c && make
  liftIO $ D.withCurrentDirectory "c" $ C.Unix.linkeach (C.upupsrcdir config </> "c")
  pbs <- liftIO $ pbchunksrc $ showMach $ C.m config
  liftIO $ D.withCurrentDirectory "c" $ C.Unix.linkList ("../boot" </> showMach m) $ pbs
  U.withCurrentDirectory "c" $ C.Unix.build config j
  -- cd s && make bootstrap
  liftIO $ D.withCurrentDirectory "s" $ C.Unix.linkeach (C.upupsrcdir config </> "s")
  U.withCurrentDirectory "s" $ S.Unix.build config

installKernelTarget = "installkernellib"

install :: String -> FilePath -> C.Config -> Run ()
install = I.install
