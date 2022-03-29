{-# LANGUAGE RecordWildCards #-}

module C.Unix(build, linkeach, linkList) where

import qualified System.Directory as D
import System.Posix.Files
import Development.Rattle
import Development.Shake.Command
import Development.Shake
import Data.Maybe
import Control.Monad.Extra
import qualified Config as C
import qualified C.Base as B
import Mach
import System.FilePath
import Debug.Trace as Trace
import System.Exit

-- were in base but 

-- cpu = "X86_64"

mdclib ncurseslib = ["-lm", "-ldl", ncurseslib, "-lpthread", "-lrt", "-luuid"]
-- ccFlags c@C.Config{..} = cppflags ++ ["-m64", "-msse2", "-Wpointer-arith", "-Wall", "-Wextra", "-Werror", "-Wno-implicit-fallthrough", "-O2", "-D_REENTRANT", "-pthread"] ++ cflags -}

ccFlags c@C.Config{..} = cppFlags ++ cFlags ++ warningFlags 

 
o = "o"
mdsrc C.Config{..} = mdarchsrc <.> "c"
mdobj C.Config{..} = mdarchsrc <.> "o"

include m = B.include m
kernelLib m = B.kernelLib m
kernelO m = B.kernelO m o
-- IO 
kernelObj m c = B.kernelObj m o [mdobj c]
mainObj = B.mainObj o
main m = B.main m o
scheme m = B.scheme m
kernel m = B.kernel m

buildObj :: C.Config -> FilePath -> Run ()
buildObj c@C.Config{..} cf = cmd $ [cc] ++ ccFlags c ++ ["-c"] ++ mdcppFlags ++ ["-D" ++ show cpu, "-I" ++ (include $ showMach m) , zlibInc, lz4Inc, cf]

kernelOTarget :: C.Config -> Int -> Run ()
kernelOTarget config@C.Config{..} j = withCmdOptions [AddEnv "SCHEMEHEAPDIRS" $ "../boot" </> showMach m] $ do
  pbsrc <- liftIO $ B.pbchunksrc $ showMach m
  mapM_ (buildObj config) $ (mdsrc config):B.kernelsrc ++ pbsrc -- build kernelobjs
  unless (zlibDep == "") $ do
    cmd (Cwd "../zlib") (AddEnv "CFLAGS" $ unwords cFlags) Shell $ zlibConfigureEnv ++ ["./configure"] ++ zlibConfigureFlags
    cmd (Cwd "../zlib") ["make", "-j", show j]
  unless (lz4Dep == "") $
    lz4LibTarget config j
  kos <- liftIO $ kernelObj (showMach m) config
  cmd $ [ld, "-r", "-X"] ++ mdldFlags ++ ["-o", kernelO $ showMach m] ++ kos ++ [zlibLib, lz4Lib]
  
kernelLibTarget :: C.Config -> Run ()
kernelLibTarget config@C.Config{..} = withCmdOptions [AddEnv "SCHEMEHEAPDIRS" $ "../boot" </> showMach m] $ do
  pbsrc <- liftIO $ B.pbchunksrc $ showMach m
  mapM_ (buildObj config) $ (mdsrc config):B.kernelsrc ++ pbsrc -- build kernelobjs
  kos <- liftIO $ kernelObj (showMach m) config
  cmd $ [ar] ++ arFlags ++ [kernelLib $ showMach m] ++ kos -- run ar on those object files we just built

-- choose between KernelLibTarget and KernelOTarget
kernelTarget :: C.Config -> Int -> Run ()
kernelTarget c@C.Config{..} j = do
  case kernel of
    "KernelLib" -> kernelLibTarget c
    "KernelO" -> kernelOTarget c j
    _ -> liftIO $ die $ "Unrecognized kernel target: " ++ show kernel

mainTarget :: C.Config -> Run ()
mainTarget config@C.Config{..} = do 
  buildObj config B.mainsrc -- build mainobj
  cmd ["/bin/cp", "-p", mainObj, main $ showMach m]

-- always ../lz4/lib/liblz4.a"
lz4LibTarget :: C.Config -> Int -> Run ()
lz4LibTarget C.Config{..} j = do
  cmd (Cwd "../lz4/lib") (AddEnv "CC" cc) (AddEnv "CPPFLAGS" $ unwords cppFlags) (AddEnv "AR" ar) (AddEnv "ARFLAGS" $ unwords arFlags) (AddEnv "RANLIB" ranlib) (AddEnv "CFLAGS" $ unwords cFlags) (AddEnv "LDFLAGS" $ unwords ldFlags) ["make", "liblz4.a", "-j", show j]

-- kernel kernellinkdeps main
schemeTarget :: C.Config -> Int -> Run ()
schemeTarget c@C.Config{..} j = do
  (kernelTmp, kernelLinkLibsTmp) <- case kernel of
                                      "KernelLib" -> do
                                        cmd (Cwd "../zlib") (AddEnv "CFLAGS" $ unwords cFlags) ["./configure"]
                                        cmd (Cwd "../zlib") ["make", "-j", show j]
                                        lz4LibTarget c j
                                        pure (kernelLib $ showMach m, [zlibLib, lz4Lib])
                                      "KernelO" -> pure (kernelO $ showMach m, [])
  kernelTarget c j
  mainTarget c
  -- $C ${mdlinkflags} -o ${Scheme} ${Main} ${Kernel} ${KernelLinkLibs} ${LDFLAGS} ${LIBS}
  unless (exePreStep == "") $
    cmd Shell exePreStep
  cmd $ [cc] ++ ccFlags c ++ mdlinkFlags ++ ["-o", scheme (showMach m) exeSuffix, main $ showMach m, kernelTmp] ++ kernelLinkLibsTmp ++ ldFlags ++ libs
  when exePostStep $
    cmd ["paxctl", "+m", scheme (showMach m) exeSuffix]

linkeach :: FilePath -> IO ()
linkeach dir = do
  files <- D.listDirectory $ dir
  let workln src dest = whenM (andM [D.doesFileExist src, notM $ D.doesFileExist dest]) $
                        createSymbolicLink src dest
  mapM_ (\x -> workln (dir </> x) x) files

linkList :: FilePath -> [FilePath] -> IO ()
linkList dir ls = do
  let workln src dest = whenM (andM [D.doesFileExist src, notM $ D.doesFileExist dest]) $
                        createSymbolicLink src dest
  mapM_ (\x -> workln (dir </> x) x) ls 

-- doit target
build :: C.Config -> Int -> Run ()
build config@C.Config{..} j = do
  -- make sure we link the source files first.
  -- zlibConfig <- liftIO $ withCurrentDirectory "zlib" $ Zlib.Config.config $ cFlags ++ ["-m64"]
  liftIO $ putStrLn "building scheme target"
  schemeTarget config j
