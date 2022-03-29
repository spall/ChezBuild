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
kernelObj cd m c = B.kernelObj cd m o [mdobj c]
mainObj = B.mainObj o
main m = B.main m o
scheme m = B.scheme m
kernel m = B.kernel m

buildObj :: FilePath -> C.Config -> FilePath -> Run ()
buildObj cd c@C.Config{..} cf = cmd (Cwd cd) $ [cc] ++ ccFlags c ++ ["-c"] ++ mdcppFlags ++ ["-D" ++ show cpu, "-I" ++ (include $ showMach m) , zlibInc, lz4Inc, cf]

kernelOTarget :: FilePath -> C.Config -> Int -> Run ()
kernelOTarget cd config@C.Config{..} j = withCmdOptions [AddEnv "SCHEMEHEAPDIRS" $ "../boot" </> showMach m] $ do
  pbsrc <- liftIO $ B.pbchunksrc cd $ showMach m
  mapM_ (buildObj cd config) $ (mdsrc config):B.kernelsrc ++ pbsrc -- build kernelobjs
  unless (zlibDep == "") $ do
    cmd (AddEnv "CFLAGS" $ unwords cFlags) (Cwd $ cd </> "../zlib") Shell $ zlibConfigureEnv ++ ["./configure"] ++ zlibConfigureFlags
    cmd (Cwd $ cd </> "../zlib") ["make", "-j", show j]
  unless (lz4Dep == "") $
    lz4LibTarget cd config j
  kos <- liftIO $ kernelObj cd (showMach m) config
  cmd (Cwd cd) $ [ld, "-r", "-X"] ++ mdldFlags ++ ["-o", kernelO $ showMach m] ++ kos ++ [zlibLib, lz4Lib]
  
kernelLibTarget :: FilePath -> C.Config -> Run ()
kernelLibTarget cd config@C.Config{..} = withCmdOptions [AddEnv "SCHEMEHEAPDIRS" $ "../boot" </> showMach m] $ do
  pbsrc <- liftIO $ B.pbchunksrc cd $ showMach m
  mapM_ (buildObj cd config) $ (mdsrc config):B.kernelsrc ++ pbsrc -- build kernelobjs
  kos <- liftIO $ kernelObj cd (showMach m) config
  cmd (Cwd cd) $ [ar] ++ arFlags ++ [kernelLib $ showMach m] ++ kos -- run ar on those object files we just built

-- choose between KernelLibTarget and KernelOTarget
kernelTarget :: FilePath -> C.Config -> Int -> Run ()
kernelTarget cd c@C.Config{..} j = do
  case kernel of
    "KernelLib" -> kernelLibTarget cd c
    "KernelO" -> kernelOTarget cd c j
    _ -> liftIO $ die $ "Unrecognized kernel target: " ++ show kernel

mainTarget :: FilePath -> C.Config -> Run ()
mainTarget cd config@C.Config{..} = do 
  buildObj cd config B.mainsrc -- build mainobj
  cmd (Cwd cd) $ ["/bin/cp", "-p", mainObj, main $ showMach m]

-- always ../lz4/lib/liblz4.a"
lz4LibTarget :: FilePath -> C.Config -> Int -> Run ()
lz4LibTarget cd C.Config{..} j = cmd(AddEnv "CC" cc) (AddEnv "CPPFLAGS" $ unwords cppFlags) (AddEnv "AR" ar) (AddEnv "ARFLAGS" $ unwords arFlags) (AddEnv "RANLIB" ranlib) (AddEnv "CFLAGS" $ unwords cFlags) (AddEnv "LDFLAGS" $ unwords ldFlags) (Cwd $ cd </> "../lz4/lib") ["make", "liblz4.a", "-j", show j]

-- kernel kernellinkdeps main
schemeTarget :: FilePath -> C.Config -> Int -> Run ()
schemeTarget cd c@C.Config{..} j = do
  (kernelTmp, kernelLinkLibsTmp) <- case kernel of
                                      "KernelLib" -> do
                                        cmd (AddEnv "CFLAGS" $ unwords cFlags) (Cwd $ cd </> "../zlib") ["./configure"]
                                        cmd (Cwd $ cd </> "../zlib") ["make", "-j", show j]
                                        lz4LibTarget cd c j
                                        pure (kernelLib $ showMach m, [zlibLib, lz4Lib])
                                      "KernelO" -> pure (kernelO $ showMach m, [])
  kernelTarget cd c j
  mainTarget cd c
  -- $C ${mdlinkflags} -o ${Scheme} ${Main} ${Kernel} ${KernelLinkLibs} ${LDFLAGS} ${LIBS}
  unless (exePreStep == "") $
    cmd (Cwd cd) Shell exePreStep
  cmd (Cwd cd) $ [cc] ++ ccFlags c ++ mdlinkFlags ++ ["-o", scheme (showMach m) exeSuffix, main $ showMach m, kernelTmp] ++ kernelLinkLibsTmp ++ ldFlags ++ libs
  when exePostStep $
    cmd (Cwd cd) ["paxctl", "+m", scheme (showMach m) exeSuffix]

linkeach :: FilePath -> FilePath -> IO ()
linkeach cd dir = do
  files <- D.listDirectory $ cd </> dir
  let workln src dest = whenM (andM [D.doesFileExist (cd </> src), notM $ D.doesFileExist dest]) $
                        createSymbolicLink src dest
  mapM_ (\x -> workln (dir </> x) (cd </> x)) files

linkList :: FilePath -> FilePath -> [FilePath] -> IO ()
linkList cd dir ls = do
  let workln src dest = whenM (andM [D.doesFileExist (cd </> src), notM $ D.doesFileExist dest]) $
                        createSymbolicLink src dest
  mapM_ (\x -> workln (dir </> x) (cd </> x)) ls 

-- doit target
build :: FilePath -> C.Config -> Int -> Run ()
build = schemeTarget
