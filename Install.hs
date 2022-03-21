{-# LANGUAGE RecordWildCards #-}

module Install(install) where

import Development.Rattle
import System.FilePath
import System.Directory as D
import Config
import Control.Monad
import Mach

defaultInclude m = "boot" </> m
petiteBoot dir = dir </> "petite.boot"
schemeBoot dir = dir </> "scheme.boot"
revision dir = dir </> "revision"
scheme m = "bin" </> m </> "scheme"
petite m = "bin" </> m </> "petite"

installLibExamples version installLib = installLib </> version </> "examples"
installLibBin version installLib m = installLib </> version </> m

bin tempRoot installBin = tempRoot </> installBin
lib version tempRoot installLib = tempRoot ++ installLib </> version
libExamples version tempRoot il = tempRoot ++ (installLibExamples version il)
libBin version tempRoot installLib m = tempRoot ++ installLibBin version installLib m
man tempRoot installMan = tempRoot ++ installMan


petitePath tr ib installPetiteName = bin tr ib </> installPetiteName
schemePath tr ib installSchemeName = bin tr ib </> installSchemeName
schemeScriptPath tr ib installScriptName = bin tr ib </> installScriptName


installSH :: Config -> [String] -> Run ()
installSH Config{..} rest = do
  if installOwner == ""
    then if installGroup == ""
         then cmd $ ["./installsh"] ++ rest
         else cmd $ ["./installsh", "-g", "\"" ++ installGroup ++ "\""] ++ rest
    else if installGroup == ""
         then cmd $ ["./installsh", "-o", "\"" ++ installOwner ++ "\""] ++ rest
         else cmd $ ["./installsh", "-o", "\"" ++ installOwner ++ "\"", "-g", "\"" ++ installGroup ++ "\""] ++ rest

binInstall :: Config -> Run ()
binInstall c@Config{..} = do
  let sp = schemePath tempRoot installBin installSchemeName
      pp = petitePath tempRoot installBin installPetiteName
      ssp = schemeScriptPath tempRoot installBin installScriptName
  liftIO $ D.createDirectoryIfMissing True $ bin tempRoot installBin
  liftIO $ putStrLn $ "created directory : " ++ bin tempRoot installBin
  installSH c ["-m", "555", scheme $ showMach m, sp]
  cmd ["ln", "-f", sp, pp]
  cmd ["ln", "-f", sp, ssp]
  

libBinInstall :: String -> FilePath -> Config -> Run ()
libBinInstall version include c@Config{..} = do
  let lb = libBin version tempRoot installLib $ showMach m
  liftIO $ D.createDirectoryIfMissing True lb
  installSH c ["-m", "444", petiteBoot include, lb </> "petite.boot"]
  when (not $ installPetiteName == "petite") $
    cmd ["ln", "-f", lb </> "petite.boot", lb </> installPetiteName <.> "boot"]
  installSH c ["-m", "444", schemeBoot include, lb </> "scheme.boot"]
  when (not $ installSchemeName == "scheme") $
    cmd ["ln", "-f", lb </> "scheme.boot", lb </> installSchemeName <.> "boot"]
  cmd ["ln", "-f", lb </> "scheme.boot", lb </> installScriptName <.> "boot"]
  installSH c ["-m", "444", defaultInclude $ showMach m </> "main.o", lb]
  installSH c ["-m", "444", include </> "scheme.h", lb]
  installSH c ["-m", "444", revision include, lb </> "revision"]

-- scheme.1 and petite.1 
dotOne :: String -> String -> Config -> Run ()
dotOne version str c@Config{..} = 
  cmd Shell ["sed", "-e", "\"s;" ++ installBin ++ ";" ++ installBin ++ ";g\""
      ,"-e", "\"s;" ++ installLibExamples version installLib ++ ";" ++ installLibExamples version installLib ++ ";g\""
      ,"-e", "\"s;" ++ installLibBin version installLib (showMach m) ++ ";" ++ installLibBin version installLib (showMach m) ++ ";g\""
      ,"-e", "\"s;" ++ installPetiteName ++ ";" ++ installPetiteName ++ ";g\""
      ,"-e", "\"s;" ++ installSchemeName ++ ";" ++ installSchemeName ++ ";g\""
      ,"-e", "\"s;" ++ installScriptName ++ ";" ++ installScriptName ++ ";g\""
      ,"scheme.1.in", ">", str <.> "1"]

manInstall :: String -> Config -> Run ()
manInstall version c@Config{..} = do
  let man_ = man tempRoot installMan
  liftIO $ D.createDirectoryIfMissing True man_
  dotOne version "scheme" c 
  dotOne version "petite" c
  -- installSH c ["-d", "-m", "755", man_]
  installSH c ["-m", "444", "scheme.1", man_ </> installSchemeName <.> "1"]
  when gzipManPages $
    cmd ["gzip", "-f", man_ </> installSchemeName <.> "1"]
  installSH c ["-m", "444", "petite.1", man_ </> installPetiteName <.> "1"]
  when gzipManPages $
    cmd ["gzip", "-f", man_ </> installPetiteName <.> "1"]
  
libLibInstall :: String -> Config -> Run ()
libLibInstall version c@Config{..} = do
  let libe = libExamples version tempRoot installLib
  liftIO $ D.createDirectoryIfMissing True libe
  installSH c ["-m", "444", "examples/*", libe]

installKernelObj :: String -> Config -> Run ()
installKernelObj version c@Config{..} = do
  let lb = libBin version tempRoot installLib $ showMach m
  liftIO $ D.createDirectoryIfMissing True lb
  installSH c ["-m", "444", defaultInclude $ showMach m </> "kernel.o", lb]

installzLib :: String -> Config -> Run ()
installzLib version c@Config{..} = do
  let lb = libBin version tempRoot installLib $ showMach m
  liftIO $ D.createDirectoryIfMissing True lb
  installSH c ["-m", "444", "zlib/libz.a", lb]

installlz4 :: String -> Config -> Run ()
installlz4 version c@Config{..} = do
  let lb = libBin version tempRoot installLib $ showMach m
  liftIO $ D.createDirectoryIfMissing True lb
  installSH c ["-m", "444", "lz4/lib/liblz4.a", lb]

installKernelLib :: String -> Config -> Run ()
installKernelLib version c@Config{..} = do
  let lb = libBin version tempRoot installLib $ showMach m
  liftIO $ D.createDirectoryIfMissing True lb
  -- installzlibtarget
  when installzLibTarget $
    installzLib version c
  -- installlz4target
  when installlz4Target $
    installlz4 version c
  installSH c ["-m", "444", defaultInclude $ showMach m </> "libkernel.a", lb]

installKernelTarget :: String -> Config -> Run ()
installKernelTarget version c = case kernel c of
                                  "KernelLib" -> installKernelLib version c
                                  "KernelO" -> installKernelObj version c

-- include is where we put the boot files etc which will depend on when bootstrap succeeded.
-- normally is boot/m/
install :: String -> FilePath -> Config -> Run ()
install version include config = do
  binInstall config
  libBinInstall version include config
  manInstall version config
  libLibInstall version config
  installKernelTarget version config
  
