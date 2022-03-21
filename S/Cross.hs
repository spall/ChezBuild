{-# LANGUAGE RecordWildCards #-}

module S.Cross where

import S.Base
import Mach
import qualified System.Directory as D
import qualified S.Config as S
import Development.Rattle
import System.FilePath
import System.Exit

-- defaults in the Mf-Cross makefile
what = ["all", "examples"]
base = "../.."
o = 2
i = True
d = 3
xpatch = "xpatch"
xpatchobj = patchobj -- exported by S.Base

--xdoit just calls xboot
-- om is pb because we already did the correct ocnfigure so c contains the m we want to cross to
xboot :: FilePath -> FilePath -> String -> Mach -> Integer -> Integer -> S.Config -> S.Config -> Run ()
xboot cd base what om o d c@S.Config{..} c2 = do
  let dv = (S.defaultVars om){S.scheme=base</>"bin"</> "scheme"
                               ,S.schemeHeapDirs=[base </> "boot" </> showMach om]
                               ,S.o=o, S.d=d, S.patchFile="xpatch"}
  -- xpatch
  -- duh we need to build the object files here
  -- need to build them with om. 
  buildObjectFile cd (base </> "boot" </> showMach om) "." [] "cmacros.so" dv c2 -- cmacros.so
  buildObjectFile cd (base </> "boot" </> showMach om) "." ["." </> "cmacros.so"] "priminfo.so" dv c2 --priminfo.so
  mapM_ (\x -> buildObjectFile cd (base </> "boot" </> showMach om) "." ["." </> "cmacros.so", "." </> "priminfo.so"] x dv c2) ["primvars.so", "env.so", "setup.so"]
  -- need to build nanopass
  nanopassSo cd (base </> "boot" </> showMach om) "." dv c2
  mapM_ (\f -> buildPatchFile cd (base </> "boot" </> showMach om) f dv c2) xpatchobj
  cmd (Cwd cd) Shell $ ["cat"] ++ xpatchobj ++ [">", "xpatch"]
  case what of
    "all" -> do
      liftIO $ D.createDirectoryIfMissing True $ cd </> "../boot/tmp" </> showMach m
      allTarget cd (base </> "boot" </> showMach om) ("../boot/tmp" </> showMach m) dv c
    x -> liftIO $ die $ x ++ " is not supported by xboot right now"
    
-- ("../boot" </> showMach m)
