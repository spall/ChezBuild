{-# LANGUAGE RecordWildCards #-}

module S.Base(bootstrap, keepbootfiles, allTarget, patchobj
             , buildPatchFile, buildObjectFile, nanopassSo) where

import qualified System.Directory as D
import qualified S.Config as S
import System.FilePath
import System.Info.Extra
import Control.Monad.Extra
import System.Posix.Files
import System.Exit
import Data.List.Extra
import Mach

import Development.Rattle
import Development.Shake

revision m = "../boot" </> m </> "revision"

showBool :: Bool -> String
showBool True = "t"
showBool False = "f"

basesrc = ["library.ss", "prims.ss", "mathprims.ss", "record.ss", "5_1.ss", "5_2.ss"
          ,"5_3.ss", "strnum.ss", "bytevector.ss", "5_4.ss", "5_6.ss", "5_7.ss", "event.ss"
          ,"4.ss", "front.ss", "foreign.ss", "6.ss", "print.ss", "newhash.ss", "format.ss"
          ,"date.ss", "7.ss", "cafe.ss", "trace.ss", "engine.ss", "interpret.ss", "cprep.ss"
          ,"cpcheck.ss", "cp0.ss", "cpvalid.ss", "cptypes.ss", "cpcommonize.ss", "cpletrec.ss"
          ,"inspect.ss", "enum.ss", "io.ss", "read.ss", "primvars.ss", "syntax.ss", "costctr.ss"
          ,"expeditor.ss", "exceptions.ss", "pretty.ss", "env.ss", "fasl.ss", "vfasl.ss", "reloc.ss"
          ,"pdhtml.ss", "strip.ss", "ftype.ss", "back.ss"]

baseobj m = map (-<.> m) basesrc

compilersrc = ["cpnanopass.ss", "cpprim.ss", "compile.ss", "cback.ss"]
compilerobj m = map (-<.> m) compilersrc

source aic = allsrc aic ++ ["mkheader.ss"]
allsrc :: [FilePath] -> [FilePath]
allsrc archincludes = basesrc ++ compilersrc ++ ["cmacros.ss"] ++ archincludes
  ++ ["setup.ss", "debug.ss", "priminfo.ss", "primdata.ss", "layout.ss", "base-lang.ss"
     ,"expand-lang.ss", "primref.ss", "types.ss", "io-types.ss", "fasl-helpers.ss"
     ,"hashtable-types.ss", "np-languages.ss", "fxmap.ss"]

macroobj = ["cmacros.so", "priminfo.so", "primvars.so", "env.so", "setup.so"]

-- the following controls the patch files loaded before compiling, typically used only
-- to load a new compiler for cross compilation
patchobj = ["patch.patch", "cpnanopass.patch", "cpprim.patch", "cprep.patch", "cpcheck.patch"
           ,"cp0.patch", "cpvalid.patch", "cptypes.patch", "cpcommonize.patch", "cpletrec.patch"
           ,"reloc.patch", "compile.patch", "fasl.patch", "vfasl.patch", "syntax.patch", "env.patch"
           ,"read.patch", "interpret.patch", "ftype.patch", "strip.patch", "ubify.patch"]


src = basesrc ++ compilersrc
obj m = baseobj m ++ compilerobj m

-- combination of bootstrap and allx
bootstrap :: S.BaseVars -> S.Config -> Run ()
bootstrap bv config@S.Config{..} = do
  -- revision 
  cmd Shell ["./update-revision", ">", revision $ showMach m]
  -- make allx
  liftIO $ D.createDirectoryIfMissing False "a1"
  e <- allAndCheckboot ("../boot" </> showMach m) "a1" bv config
  if e then do
    pure "a1"
    doCopy (showMach m) "a1" -- "a1/scheme.boot" "a1/petite.boot" "a1/scheme.h" "a1/equates.h"
    else do
    liftIO $ D.createDirectoryIfMissing False "a2"
    e2 <- allAndCheckboot "a1" "a2" bv config
    if e2 then do
      pure "a2"
      doCopy (showMach m) "a2" -- "a2/scheme.boot" "a2/petite.boot" "a2/scheme.h" "a2/equates.h"
      else do
      liftIO $ D.createDirectoryIfMissing False "a3"
      e3 <- allAndCheckboot "a2" "a3" bv config
      if e3 then do
        doCopy (showMach m) "a3" -- "a3/scheme.boot" "a3/petite.boot" "a3/scheme.h" "a3/equates.h"
        else do
        liftIO $ die "Failed to bootstrap."


nanopassSo :: FilePath -> FilePath -> S.BaseVars -> S.Config -> Run ()
nanopassSo pd dir S.BaseVars{..} S.Config{..} = do
  cmd (AddEnv "SCHEMEHEAPDIRS" pd) Shell ["echo", "'(reset-handler abort)'"
             ,"'(base-exception-handler (lambda (c) (fresh-line) (display-condition c) (newline) (reset)))'"
             ,"'(keyboard-interrupt-handler (lambda () (display \"interrupted---aborting\n\") (reset)))'"
             ,"'(optimize-level " ++ show o ++ ")'"
             ,"'(debug-level " ++ show d ++ ")'"
             ,"'(commonization-level " ++ cl ++ ")'"
             ,"'(fasl-compressed #" ++ showBool fc ++ ")'"
             ,"'(compress-format " ++ xf ++ ")'"
             ,"'(compress-level " ++ xl ++ ")'"
             ,"'(generate-inspector-information #" ++ showBool i ++ ")'"
             ,"'(collect-trip-bytes (expt 2 24))'"
             ,"'(collect-request-handler (lambda () (collect 0 1)))'"
             ,"'(collect 1 2)'"
             ,"'(compile-library \"../nanopass/nanopass.ss\" \"" ++ dir </> "nanopass.so" ++ "\")'"
             ,"|", scheme, "-q", "--libdirs", "\"../nanopass" ++ dirsep ++ dirsep ++ ".\""
             ,"--compile-imported-libraries"]

bootall :: FilePath -> FilePath -> S.BaseVars -> S.Config -> Run ()
bootall pd dir S.BaseVars{..} S.Config{..} = do
  if patchFile == ""
    then
    cmd (AddEnv "SCHEMEHEAPDIRS" pd) (AddEnv "CHEZSCHEMELIBDIRS" $ dir ++ ":") $ [scheme, "-q"] ++ map (dir </>) macroobj ++ ["--script", dir </> "script.all"] -- patchfile goes before --script but its empty so omitted
    else
    cmd (AddEnv "SCHEMEHEAPDIRS" pd) (AddEnv "CHEZSCHEMELIBDIRS" $ dir ++ ":") $ [scheme, "-q"] ++ map (dir </>) macroobj ++ [patchFile, "--script", dir </> "script.all"]

mkheaderSo :: FilePath -> FilePath -> S.BaseVars -> S.Config -> Run ()
mkheaderSo pd dir = do
  buildObjectFile pd dir [dir </> "cmacros.so", dir </> "priminfo.so", dir </> "primvars.so", dir </> "env.so"] "mkheader.so"

cequatesTarget :: FilePath -> FilePath -> S.BaseVars -> S.Config -> Run ()
cequatesTarget dir pd
  = buildHeaderFile pd dir (macroobj++["mkheader.so"]) (dir </> "equates.h") (pd </> "equates.h")

cheaderTarget :: FilePath -> FilePath -> S.BaseVars -> S.Config -> Run ()
cheaderTarget dir pd S.BaseVars{..} S.Config{..} = do
  let objs = (macroobj++["mkheader.so"])
      file = (dir </> "scheme.h")
      bak = (pd </> "scheme.h")
  cmd (AddEnv "SCHEMEHEAPDIRS" pd) (AddEnv "CHEZSCHEMELIBDIRS" $ dir++":.") Shell $
    ["echo", "'(reset-handler abort) (mk" ++ takeFileName file ++ " \"" ++ file ++ "\" (quote " ++ showMach m ++ "))'"
    ,"|", scheme, "-q"] ++ map (dir </>) objs
    ++ ["&&", "(if", "`cmp", "-s", file, bak ++ "`;", "then", "cp", "-p", bak, file ++ ";", "fi)"]

cgcoceTarget :: FilePath -> FilePath -> S.BaseVars -> S.Config -> Run ()
cgcoceTarget dir pd
  = buildHeaderFile pd dir (macroobj ++ ["mkheader.so", "mkgc.so"]) (dir </> "gc-oce.inc") (pd </> "gc-oce.inc")

cgcocdTarget :: FilePath -> FilePath -> S.BaseVars -> S.Config -> Run ()
cgcocdTarget dir pd
  = buildHeaderFile pd dir (macroobj ++ ["mkheader.so", "mkgc.so"]) (dir </> "gc-ocd.inc") (pd </> "gc-ocd.inc")

cgcparTarget :: FilePath -> FilePath -> S.BaseVars -> S.Config -> Run ()
cgcparTarget dir pd
  = buildHeaderFile pd dir (macroobj ++ ["mkheader.so", "mkgc.so"]) (dir </> "gc-par.inc") (pd </> "gc-par.inc")

cheapcheckTarget :: FilePath -> FilePath -> S.BaseVars -> S.Config -> Run ()
cheapcheckTarget dir pd
  = buildHeaderFile pd dir (macroobj ++ ["mkheader.so", "mkgc.so"]) (dir </> "heapcheck.inc") (pd </> "heapcheck.inc")
  
mkgcSo :: FilePath -> FilePath -> S.BaseVars -> S.Config -> Run ()
mkgcSo pd dir = buildObjectFile pd dir [dir </> "cmacros.so", dir </> "priminfo.so", dir </> "primvars.so", dir </> "env.so", dir </> "mkheader.so"] "mkgc.so"
  

allTarget :: FilePath -> FilePath -> S.BaseVars -> S.Config -> Run ()
allTarget pd dir bv c = do
  -- build all macroobj
  buildObjectFile pd dir [] "cmacros.so" bv c -- cmacros.so
  buildObjectFile pd dir [dir </> "cmacros.so"] "priminfo.so" bv c --priminfo.so
  mapM_ (\x -> buildObjectFile pd dir [dir </> "cmacros.so", dir </> "priminfo.so"] x bv c) ["primvars.so", "env.so", "setup.so"]
  -- 
  nanopassSo pd dir bv c
  makescript pd dir bv c
  -- 
  bootall pd dir bv c
  liftIO $ putStrLn "did bootall"
  -- 
  mkheaderSo pd dir bv c
  -- build mkgc.so
  mkgcSo pd dir bv c
  --
  cheaderTarget dir pd bv c
  cequatesTarget dir pd bv c
  cgcocdTarget dir pd bv c
  cgcoceTarget dir pd bv c
  cgcparTarget dir pd bv c
  cheapcheckTarget dir pd bv c


makescript :: FilePath -> FilePath -> S.BaseVars -> S.Config -> Run ()
makescript pd dir S.BaseVars{..} S.Config{..} = do
  cmd (AddEnv "SCHEMEHEAPDIRS" pd) (AddEnv "CHEZSCHEMELIBDIRS" $ dir++":.") Shell
    ["echo", "'(reset-handler abort)'"
    ,"'(for-each load (command-line-arguments))'"
    ,"'(optimize-level " ++ show o ++ ")'"
    ,"'(debug-level " ++ show d ++ ")'"
    ,"'(commonization-level " ++ cl ++ ")'"
    ,"'(fasl-compressed #" ++ showBool fc ++ ")'"
    ,"'(compress-format " ++ xf ++ ")'"
    ,"'(compress-level " ++ xl ++ ")'"
    ,"'(when #" ++ showBool p ++ " (compile-profile (quote source)))'"
    ,"'(when #" ++ showBool bp ++ " (compile-profile (quote block)))'"
    ,"'(when #" ++ showBool loadspd ++ " (profile-load-data \"" ++ profileDumpSource ++ "\"))'"
    ,"'(when #" ++ showBool loadbpd ++ " (profile-load-data \"" ++ profileDumpBlock ++ "\"))'"
    ,"'(generate-inspector-information #" ++ showBool i ++ ")'"
    ,"'(generate-allocation-counts #" ++ showBool gac ++ ")'"
    ,"'(generate-instruction-counts #" ++ showBool gic ++ ")'"
    ,"'(#%$enable-pass-timing  #" ++ showBool pps ++ ")'"
    ,"'(generate-covin-files #" ++ showBool c ++ ")'"
    ,"'(run-cp0 (lambda (cp0 x) (do ([i " ++ show cp0 ++ " (fx- i 1)] [x x (cp0 x)]) ((fx= i 0) x))))'"
    ,"'(collect-trip-bytes (expt 2 24))'"
    ,"'(collect-request-handler (lambda () (collect 0 1)))'"
    ,"'(for-each (lambda (x) (delete-file (string-append (path-root (symbol->string x)) \".covin\"))) (quote (" ++ (unwords $ map (dir </>) (obj $ showMach m)) ++ ")))'"
    ,"'(time (for-each (lambda (x y) (collect 1 2) (" ++ compile ++ " (symbol->string x) (symbol->string y) (quote " ++ showMach m ++ "))) (quote (" ++ unwords src ++ ")) (quote (" ++ (unwords $ map (dir </>) (obj $ showMach m)) ++ "))))'"
    ,"'(printf \"    ~a bytes peak memory use~n\" (maximum-memory-bytes))'"
    ,"'(when #" ++ showBool pps ++ " (#%$print-pass-stats))'"
    ,"'(delete-file (string-append (path-root \"" ++ dir </> "petite.boot" ++ "\") \".covin\"))'"
    ,"'(apply #%$make-boot-file \"" ++ dir </> "petite.boot" ++ "\" (quote " ++ showMach m ++ ") (quote ()) (map symbol->string (quote (" ++ (unwords $ map (dir </>) (baseobj $ showMach m)) ++ "))))'"
    ,"'(delete-file (string-append (path-root \"" ++ dir </> "scheme.boot" ++ "\") \".covin\"))'"
    ,"'(apply #%$make-boot-file \"" ++ dir </> "scheme.boot" ++ "\" (quote " ++ showMach m ++ ") (quote (\"petite\")) (map symbol->string (quote (" ++ (unwords $ map (dir </>) (compilerobj $ showMach m)) ++ "))))'"
    ,"'(when #" ++ showBool pdhtml ++ " (profile-dump-html))'"
    ,"'(when #" ++ showBool dumpspd ++ " (profile-dump-data \"" ++ profileDumpSource ++ "\"))'"
    ,"'(when #" ++ showBool dumpbpd ++ " (profile-dump-data \"" ++ profileDumpBlock ++ "\"))'"
    ,">", dir </> "script.all"]

buildPatchFile :: FilePath -> FilePath -> S.BaseVars -> S.Config -> Run ()
buildPatchFile shd pf S.BaseVars{..} S.Config{..} = do
  cmd (AddEnv "SCHEMEHEAPDIRS" shd) (AddEnv "CHEZSCHEMELIBDIRS" "..:") Shell $
    ["echo", "'(reset-handler abort)'"
    ,"'(optimize-level " ++ show o ++ ")'"
    ,"'(debug-level " ++ show d ++ ")'"
    ,"'(commonization-level " ++ cl ++ ")'"
    ,"'(fasl-compressed #" ++ showBool fc ++ ")'"
    ,"'(compress-format " ++ xf ++ ")'"
    ,"'(compress-level " ++ xl ++ ")'"
    ,"'(when #" ++ showBool xp ++ " (compile-profile (quote source)))'"
    ,"'(when #" ++ showBool xbp ++ " (compile-profile (quote block)))'"
    ,"'(generate-inspector-information #" ++ showBool i ++ ")'"
    ,"'(run-cp0 (lambda (cp0 x)'"
    ,"'           (do ([i " ++ show cp0 ++ " (fx- i 1)] [x x (cp0 x)])'"
    ,"'               ((fx= i 0) x))))'"
    ,"'(collect-trip-bytes (expt 2 24))'"
    ,"'(collect-request-handler (lambda () (collect 0 1)))'"
    ,"'(collect 1 2)'"
    ,"'(time (" ++ compile ++ " \"" ++ pf -<.> "ss" ++ "\" \"" ++ pf ++ "\" (quote " ++ showMach m ++ ")))'"
    ,"'(printf \"    ~a bytes peak memory use~n\" (maximum-memory-bytes))'"
    ,"| " ++ scheme ++ " -q "] ++ macroobj

buildHeaderFile :: FilePath -> FilePath -> [FilePath] -> FilePath -> FilePath -> S.BaseVars -> S.Config -> Run ()
buildHeaderFile pd dir objs file bak S.BaseVars{..} S.Config{..} = do
  cmd (AddEnv "SCHEMEHEAPDIRS" pd) (AddEnv "CHEZSCHEMELIBDIRS" $ dir++":.") Shell $
    ["echo", "'(reset-handler abort) (mk" ++ takeFileName file ++ " \"" ++ file ++ "\")'"
    ,"|", scheme, "-q"] ++ map (dir </>) objs
    ++ ["&&", "(if", "`cmp", "-s", file, bak ++ "`;", "then", "cp", "-p", bak, file ++ ";", "fi)"]


buildObjectFile :: FilePath -> FilePath -> [FilePath] -> FilePath -> S.BaseVars -> S.Config -> Run ()
buildObjectFile shd dir ls so S.BaseVars{..} S.Config{..} = do
  liftIO $ putStrLn shd
  cmd (AddEnv "SCHEMEHEAPDIRS" shd) (AddEnv "CHEZSCHEMELIBDIRS" (shd ++ ":.")) Shell $ -- used to be ..
        ["echo", "'(reset-handler abort)'"
        ,"'(base-exception-handler (lambda (c) (fresh-line) (display-condition c) (newline) (reset)))'"
        ,"'(keyboard-interrupt-handler (lambda () (display \"interrupted---aborting\\n\") (reset)))'"
        ,"'(optimize-level " ++ show o ++ ")'"
        ,"'(debug-level " ++ show d ++ ")'"
        ,"'(commonization-level " ++ cl ++ ")'"
        ,"'(fasl-compressed #" ++ showBool fc ++ ")'"
        ,"'(compress-format " ++ xf ++ ")'"
        ,"'(compress-level " ++ xl ++ ")'"
        ,"'(generate-inspector-information #" ++ showBool i ++ ")'"
        ,"'(subset-mode (quote system))'"
        ,"'(compile-file \""  ++ so-<.>"ss" ++ "\" \"" ++ dir </> so ++ "\")'"
        ,"|", scheme, "-q"] ++ ls

checkboot :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> S.BaseVars -> S.Config -> Run Bool
checkboot dir psb ppb schemeBoot petiteBoot S.BaseVars{..} S.Config{..} = do
  cmd (AddEnv "SCHEMEHEAPDIRS" $dir++":.") (AddEnv "CHEZSCHEMELIBDIRS" $dir++":.") Shell ["(", "echo", "'(reset-handler abort)'"
                         ,"'(base-exception-handler (lambda (c) (fresh-line) (display-condition c) (newline) (reset)))'"
                         ,"'(begin'"
                         ,"'(#%$fasl-file-equal? \"" ++ psb ++ "\"", "\"" ++ schemeBoot ++ "\"", "#t)'"
                         ,"'(#%$fasl-file-equal? \"" ++ ppb ++ "\"", "\"" ++ petiteBoot ++ "\"", "#t)'"
                         ,"'(printf \"bootfile comparison succeeded\n\"))'"
                         ,"|", "../bin" </> showMach m </> "scheme" ++ exeSuffix, "-b", ppb, "-q", ")", ";", "echo", "$?", ">", dir </> "attempt1.ec"]

  e1 <- liftIO $ readFile $ dir </> "attempt1.ec"
  return $ (trim e1) == "0"

-- does all + checkboot
allAndCheckboot :: FilePath -> FilePath -> S.BaseVars -> S.Config -> Run Bool
allAndCheckboot pd dir bv c@S.Config{..} = do
  let psb = pd </> "scheme.boot"
      ppb = pd </> "petite.boot"
      schemeBoot = dir </> "scheme.boot"
      petiteBoot = dir </> "petite.boot"
      
  allTarget pd dir bv c
  checkboot dir psb ppb schemeBoot petiteBoot bv c

doCopy :: String -> FilePath -> Run ()
doCopy m dir = do
  let bootFiles = ["scheme.boot", "petite.boot", "scheme.h", "equates.h", "gc-oce.inc", "gc-ocd.inc", "gc-par.inc", "heapcheck.inc"]
  liftIO $ D.createDirectoryIfMissing True $ "../../boot-final" </> m
  mapM_ f bootFiles
    where f x = cmd ["cp", "-p", dir </> x, "../../boot-final" </> m </> x]

keepbootfiles :: FilePath -> S.Config -> Run ()
keepbootfiles dir S.Config{..} = do
  liftIO $ D.createDirectoryIfMissing True $ "../../boot" </> showMach m
  let boots = ["scheme.boot", "petite.boot", "scheme.h", "equates.h", "gc-oce.inc", "gc-ocd.inc", "gc-par.inc", "heapcheck.inc"]
      f x = cmd ["cp", dir </> x, "../../boot" </> showMach m </> x]
  mapM_ f boots -- could be in parallel
        
