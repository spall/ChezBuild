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


installSH :: FilePath -> Config -> [String] -> Run ()
installSH cd Config{..} rest = do
  if installOwner == ""
    then if installGroup == ""
         then cmd (Cwd cd) $ ["./installsh"] ++ rest
         else cmd (Cwd cd) $ ["./installsh", "-g", "\"" ++ installGroup ++ "\""] ++ rest
    else if installGroup == ""
         then cmd (Cwd cd) $ ["./installsh", "-o", "\"" ++ installOwner ++ "\""] ++ rest
         else cmd (Cwd cd) $ ["./installsh", "-o", "\"" ++ installOwner ++ "\"", "-g", "\"" ++ installGroup ++ "\""] ++ rest

binInstall :: FilePath -> Config -> Run ()
binInstall cd c@Config{..} = do
  let sp = schemePath tempRoot installBin installSchemeName
      pp = petitePath tempRoot installBin installPetiteName
      ssp = schemeScriptPath tempRoot installBin installScriptName
  liftIO $ D.createDirectoryIfMissing True $ cd </> (bin tempRoot installBin)
  liftIO $ putStrLn $ "created directory : " ++ bin tempRoot installBin
  installSH cd c ["-m", "555", scheme $ showMach m, sp]
  cmd (Cwd cd) ["ln", "-f", sp, pp]
  cmd (Cwd cd) ["ln", "-f", sp, ssp]
  

libBinInstall :: FilePath -> String -> FilePath -> Config -> Run ()
libBinInstall cd version include c@Config{..} = do
  let lb = libBin version tempRoot installLib $ showMach m
  liftIO $ D.createDirectoryIfMissing True $ cd </> lb
  installSH cd c ["-m", "444", petiteBoot include, lb </> "petite.boot"]
  when (not $ installPetiteName == "petite") $
    cmd (Cwd cd) ["ln", "-f", lb </> "petite.boot", lb </> installPetiteName <.> "boot"]
  installSH cd c ["-m", "444", schemeBoot include, lb </> "scheme.boot"]
  when (not $ installSchemeName == "scheme") $
    cmd (Cwd cd) ["ln", "-f", lb </> "scheme.boot", lb </> installSchemeName <.> "boot"]
  cmd (Cwd cd) ["ln", "-f", lb </> "scheme.boot", lb </> installScriptName <.> "boot"]
  installSH cd c ["-m", "444", defaultInclude $ showMach m </> "main.o", lb]
  installSH cd c ["-m", "444", include </> "scheme.h", lb]
  installSH cd c ["-m", "444", revision include, lb </> "revision"]

-- scheme.1 and petite.1 
dotOne :: FilePath -> String -> String -> Config -> Run ()
dotOne cd version str c@Config{..} = 
  cmd (Cwd cd) Shell ["sed", "-e", "\"s;" ++ installBin ++ ";" ++ installBin ++ ";g\""
      ,"-e", "\"s;" ++ installLibExamples version installLib ++ ";" ++ installLibExamples version installLib ++ ";g\""
      ,"-e", "\"s;" ++ installLibBin version installLib (showMach m) ++ ";" ++ installLibBin version installLib (showMach m) ++ ";g\""
      ,"-e", "\"s;" ++ installPetiteName ++ ";" ++ installPetiteName ++ ";g\""
      ,"-e", "\"s;" ++ installSchemeName ++ ";" ++ installSchemeName ++ ";g\""
      ,"-e", "\"s;" ++ installScriptName ++ ";" ++ installScriptName ++ ";g\""
      ,"scheme.1.in", ">", str <.> "1"]

manInstall :: FilePath -> String -> Config -> Run ()
manInstall cd version c@Config{..} = do
  let man_ = man tempRoot installMan
  liftIO $ D.createDirectoryIfMissing True $ cd </> man_
  dotOne cd version "scheme" c 
  dotOne cd version "petite" c
  -- installSH c ["-d", "-m", "755", man_]
  installSH cd c ["-m", "444", "scheme.1", man_ </> installSchemeName <.> "1"]
  when gzipManPages $
    cmd (Cwd cd) ["gzip", "-f", man_ </> installSchemeName <.> "1"]
  installSH cd c ["-m", "444", "petite.1", man_ </> installPetiteName <.> "1"]
  when gzipManPages $
    cmd (Cwd cd) ["gzip", "-f", man_ </> installPetiteName <.> "1"]
  
libLibInstall :: FilePath -> String -> Config -> Run ()
libLibInstall cd version c@Config{..} = do
  let libe = libExamples version tempRoot installLib
  liftIO $ D.createDirectoryIfMissing True $ cd </> libe
  installSH cd c ["-m", "444", "examples/*", libe]

installKernelObj :: FilePath -> String -> Config -> Run ()
installKernelObj cd version c@Config{..} = do
  let lb = libBin version tempRoot installLib $ showMach m
  liftIO $ D.createDirectoryIfMissing True $ cd </> lb
  installSH cd c ["-m", "444", defaultInclude $ showMach m </> "kernel.o", lb]

installzLib :: FilePath -> String -> Config -> Run ()
installzLib cd version c@Config{..} = do
  let lb = libBin version tempRoot installLib $ showMach m
  liftIO $ D.createDirectoryIfMissing True $ cd </> lb
  installSH cd c ["-m", "444", "zlib/libz.a", lb]

installlz4 :: FilePath -> String -> Config -> Run ()
installlz4 cd version c@Config{..} = do
  let lb = libBin version tempRoot installLib $ showMach m
  liftIO $ D.createDirectoryIfMissing True $ cd </> lb
  installSH cd c ["-m", "444", "lz4/lib/liblz4.a", lb]

installKernelLib :: FilePath -> String -> Config -> Run ()
installKernelLib cd version c@Config{..} = do
  let lb = libBin version tempRoot installLib $ showMach m
  liftIO $ D.createDirectoryIfMissing True $ cd </> lb
  -- installzlibtarget
  when installzLibTarget $
    installzLib cd version c
  -- installlz4target
  when installlz4Target $
    installlz4 cd version c
  installSH cd c ["-m", "444", defaultInclude $ showMach m </> "libkernel.a", lb]

installKernelTarget :: FilePath -> String -> Config -> Run ()
installKernelTarget cd version c = case kernel c of
                                     "KernelLib" -> installKernelLib cd version c
                                     "KernelO" -> installKernelObj cd version c

-- include is where we put the boot files etc which will depend on when bootstrap succeeded.
-- normally is boot/m/
install :: FilePath -> String -> FilePath -> Config -> Run ()
install cd version include config = do
  binInstall cd config
  libBinInstall cd version include config
  manInstall cd version config
  libLibInstall cd version config
  installKernelTarget cd version config
  
