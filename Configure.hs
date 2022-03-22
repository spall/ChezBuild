{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}

module Configure(configure, semiConfigure) where

import Mach
import Config
import qualified ConfigArgs as Args
import System.Console.CmdArgs.Explicit
import qualified System.Directory as S
import Control.Monad.Extra
import Control.Monad.IO.Class
import System.FilePath
import System.Info.Extra
import System.Exit
import System.Directory
import Development.Shake
import System.Environment
import Data.Maybe
import Data.List.Extra
import Development.Rattle
import Data.Hashable
import System.Posix.Files
import qualified S.Config as SC

-- for all directories that have a scheme.boot file.... get directory name
-- 1. get list of machs from boot directory
getMachs :: Run [String]
getMachs = do
  liftIO $ ifM (S.doesDirectoryExist "boot")
    (do
        dirs <- getDirectoryFilesIO "boot" ["//scheme.boot"]
        return $ map takeDirectory dirs)
    $ return  []

-- determine the machine type
determineM :: String -> Run (Maybe Mach, Maybe Mach, Maybe Mach, Maybe Mach, String, String)
determineM configUname = do
  -- get config_uname
  case trim configUname of
    "Linux" -> do
      (m32, m64, tm32, tm64) <- getCodes LE
      return (m32, m64, tm32, tm64, "/usr", "share/man")
    "FreeBSD" -> do
      (m32, m64, tm32, tm64) <- getCodes FB
      return (m32, m64, tm32, tm64, "/usr/local", "man")
    "OpenBSD" -> do
      (m32, m64, tm32, tm64) <- getCodes OB
      return (m32, m64, tm32, tm64, "/usr/local", "man")
    "NetBSD"  -> do
      (m32, m64, tm32, tm64) <- getCodes NB
      return (m32, m64, tm32, tm64, "/usr", "share/man") 
                    -- gzipmanpages=no
    "QNX" -> f ["uname", "-a", "|", "egrep", "\'x86\'", ">", "/dev/null", "2>&1"]
             (return (Just $ Mach False I3 QNX, Nothing, Just $ Mach True I3 QNX, Nothing, "/usr/local", "man"))
             (return (Nothing, Nothing, Nothing, Nothing, "/usr/local", "man"))
    "Darwin" -> do
      (m32, m64, tm32, tm64) <- getCodesDarwin
      return (m32, m64, tm32, tm64, "/usr/local", "share/man")
    "SunOS" -> f ["uname", "-a", "|", "egrep", "\'i386|i686|amd64|athlon|x86_64\'", ">", "/dev/null", "2>&1"]
               (return (Just $ Mach False I3 S2, Just $ Mach False A6 S2, Just $ Mach True I3 S2, Just $ Mach True A6 S2, "/usr", "share/man")) -- gzipmanpages = false
               (return (Nothing, Nothing, Nothing, Nothing, "", ""))
    "CYGWIN_NT-" -> f ["uname", "-a", "|", "egrep", "\'i386|i686|amd64|athlon|x86_64\'", ">", "/dev/null", "2>&1"]
                        (return (Just $ Mach False I3 NT, Just $ Mach False A6 NT, Just $ Mach True I3 NT, Just $ Mach True A6 NT, "/usr/local", "share/man")) -- gzipmanpages = true
                        (return (Nothing, Nothing, Nothing, Nothing, "", ""))
    _ -> liftIO $ die $ "Unrecognized system: " ++ configUname
      
initConfig :: Run Config
initConfig = do
  machs <- getMachs

  env <- liftIO $ getEnvironment -- to avoid io errors hopefully

  -- init values of configArgs fields
  let m = Nothing
      workArea = ""
      threads = True
      pb = False
      gzipManPages = True
      tempRoot = ""
      installOwner = ""
      installGroup = ""
      installBin = ""
      installLib = ""
      installMan = ""
      installSchemeName = "scheme"
      installPetiteName = "petite"
      installScriptName = "scheme-script"
      cFlagsSet = False
      disableX11 = False
      disableCurses = False
      disableIConv = False
      addFlags = True
      enableWarningFlags = False
      cc = fromMaybe "gcc" $ lookup "CC" env
      cppFlags = words $ fromMaybe "" $ lookup "CPPFLAGS" env
      cFlags = words $ fromMaybe "" $ lookup "CFLAGS" env
      ld = fromMaybe "ld" $ lookup "LD" env
      ldFlags = words $ fromMaybe "" $ lookup "LDFLAGS" env
      libs = words $ fromMaybe "" $ lookup "LIBS" env
      ar = fromMaybe "ar" $ lookup "AR" env
      arFlags = words $ fromMaybe "rc" $ lookup "ARFLAGS" env
      ranlib = fromMaybe "ranlib" $ lookup "RANLIB" env
      windres = fromMaybe "windres" $ lookup "WINDRES" env
      zlibInc = "-I../zlib"
      lz4Inc = "-I../lz4/lib"
      zlibDep = "../zlib/libz.a"
      lz4Dep = "../lz4/lib/liblz4.a"
      zlibLib = "../zlib//libz.a"
      lz4Lib = "../lz4/lib/liblz4.a"
      zLibHeaderDep = ["../zlib/zconf.h", "../zlib/zlib.h"]
      lz4HeaderDep = ["../lz4/lib/lz4.h", "../lz4/lib/lz4frame.h"]
      kernel = "KernelLib"
      installzLibTarget = True
      installlz4Target = True
      srcdir = "."
      
  let defaultWarningFlags = ["-Wpointer-arith", "-Wall", "-Wextra", "-Wno-implicit-fallthrough"]
  
  bits <- f ["uname", "-a", "|", "egrep", "\'amd64|x86_64|aarch64|arm64|ppc64|powerpc64\'", ">", "/dev/null", "2>&1"]
                (return BITS64) (return BITS32)


  -- 3. call uname to get system info
  configUname <- liftIO $ if isWindows then return "CYGWIN_NT-"
                          else do
    Stdout out <- cmd "uname"
    return out
  (m32, m64, tm32, tm64 , installPrefix, installManSuffix) <- determineM configUname

  -- 5. go through args
  Args.ConfigArgs{..} <- liftIO $ processArgs $ Args.cargs machs Args.ConfigArgs{..}
  
  let defaultM = if bits == BITS64
                 then if threads then tm64 else m64
                 else if threads then tm32 else m32
 
  when (m == Just PB) $
    liftIO $ die "Don't select pb using -m or --machine, becasue pb needs the machine as the kernel host machine. Instead, use --pb to select a pb (portable bytecode) build."
  
  --   8. if m= "" then if bits = 64 then if threads = yes then m=tm64 else m = m64
                                --   else if threads = yes then m=tm32 else m = m32

  let machineSupplied = isJust m

  let (m2 , mpbhost , flagsm) = case m of
                                    Nothing -> if pb
                                               then let tmp = if bits == BITS64 then m64 else m32 in
                                                      (Just PB , tmp , tmp)
                                           
                                               else (defaultM , Nothing , defaultM) -- m = defaultm flagsm = m
                                    x -> if pb
                                              then (Just PB, x , x)
                                              else (x, Nothing, x)

  let mstr = maybe "" showMach m2

  liftIO $ ifM (orM [S.doesFileExist ("boot" </> mstr </> "scheme.boot"), S.doesFileExist (srcdir </> "boot" </> mstr </> "scheme.boot")])
    (putStrLn $ "Configuring for " ++ mstr)
    (do
        let maybem = case m2 of
                       Nothing -> "<machine type>"
                       Just x -> showMach x
        putStrLn "No suitable machine type found in 'boot'"
        putStrLn ""
        putStrLn "Available machine types:"
        putStrLn $ unwords machs
        putStrLn ""
        unless machineSupplied $
          do
            case m2 of
              Nothing -> do
                putStrLn "If the problem is that the machine type was not inferred,"
                putStrLn "you can try"
              Just x -> do
                putStrLn $ "If the problem is that the inferred machine type " ++ showMach x ++ " is"
                putStrLn "not correct, you can try"
            putStrLn "-m=<machine type>"
            putStrLn "to specify one of the available machine types."

        whenM (S.doesFileExist $ srcdir </> "boot/pb/scheme.boot") $ do
          putStrLn ""
          putStrLn "Otherwise, the pb machine type is available, so try"
          putStrLn "   --pb"
          putStrLn $ "  make " ++ maybem ++ ".bootquick"
          putStrLn "to create the boot files using a portable-bytecode build,"
          putStrLn "and then try again."
        putStrLn ""
        case m2 of
          Nothing -> do
            putStrLn "Alternatively, If no directory in 'boot' exists for the correct"
            putStrLn "machine type, then you can use Racket v7.1 or later with"
          Just x -> do
            putStrLn $ "Alternatively, since no directory in 'boot' exists for " ++ showMach x ++ ","
            putStrLn "you can try using Racket v7.1 or later with"
        putStrLn $ "  racket rktboot/main.rkt --machine " ++ maybem
        putStrLn "to create the boot files, and then try $0 again."
        die $ "")
    
  -- 10. if installbin = "" then installbin = installprefix/bin
  let installBin2 = if installBin == "" then installPrefix </> "bin"
                   else installBin
      
  -- 11. if installlib = ....
      installLib2 = if installLib == "" then installPrefix </> "lib"
                   else installLib
                         
  -- 12. if instlalman = ....
      installMan2 = if installMan == "" then installPrefix </> installManSuffix
                   else installMan


      optFlags = ["-O2"]

      warningFlags = if (not cFlagsSet) || enableWarningFlags then defaultWarningFlags else []

  -- infer flags needed for threads
      threadFlags = maybe [] getThreadFlags flagsm
      threadLibs = maybe [] getThreadLibs flagsm


  let cFlagsTmp = if cFlagsSet
                  then cFlags
                  else maybe [] (getCFlags optFlags) flagsm

-- # architecture-specific for Mf-unix
  let cpu = getCpu $ fromJust m2
  -- basically jsut depends on what cpu is so do it based on that rather than mach
  let mdarchsrc = case cpu of
                    PORTABLE_BYTECODE -> "pb"
                    X86_64 -> "i3le"
                    I386 -> "i3le"
                    ARMV6 -> "arm32le"
                    AARCH64 -> "arm32le"
                    CPU_PPC32 -> "ppc32"

  let (cursesLib, ncursesLib) = if disableCurses then ("", "")
                             else ("-lcurses", "-lncurses")

  let (iconvLib, cppFlags2) = if disableIConv
                           then ("", cppFlags ++ ["-DDISABLE_ICONV"])
                           else ("-liconv", cppFlags)

  let ldFlags2 = if addFlags
                 then ldFlags ++ (maybe [] getldFlags flagsm)
                 else ldFlags

  let libs2 = if addFlags
              then libs ++ (maybe [] (getLibsFlags iconvLib ncursesLib cursesLib disableIConv) flagsm)
              else libs

  let libs3 = if addFlags then libs2 ++ threadLibs else libs2

  liftIO $ unlessM (S.doesFileExist (srcdir </> "nanopass/nanopass.ss")) $ 
    submodInstructions "Source in 'nanopass' is missing"

  when (not $ zlibDep == "") $ do
    liftIO $ unlessM (S.doesFileExist (srcdir </> "zlib/configure")) $ 
      submodInstructions "Source in 'zlib' is missing"

  when (not $ lz4Dep == "") $ do
    liftIO $ unlessM (S.doesFileExist (srcdir </> "lz4/lib/Makefile")) $ 
      submodInstructions "Source in 'lz4' is missing"

  liftIO $ unlessM (S.doesFileExist (srcdir </> "stex/Mf-stex")) $ 
    submodInstructions "Source in 'stex' is missing" 
          
  -- compile flags for c/Mf-unix and mats/Mf-unix
  let mdcFlags = maybe [] getmdcFlags flagsm
  let mdinclude = maybe [] getmdIncludes flagsm

  -- mdlinkflags
  let mdlinkFlags = case flagsm of
                      Just (Mach _ I3 QNX) -> ["-Wl,--export-dynamic"]
                      Just (Mach _ PPC32 LE) -> ["-rdynamic"]
                      _ -> []

  let mdldFlags = maybe [] getmdldFlags flagsm

  let zlibConfigureFlags = maybe [] getzlibConfigFlags flagsm

  --9. if w = "" then w = m
  let w = if workArea == "" then showMach $ fromJust m2 else workArea
  

  let upsrcdir = if isAbsolute srcdir then srcdir else ".." </> srcdir

  
  cmd [srcdir </> "workarea", mstr, w, maybe "" showMach mpbhost]

  -- write to files
  liftIO $ writeFile (w </> "c/next_config.h") $ "#define SCHEME_SCRIPT \"" ++ installScriptName ++ "\""
    ++ "\n #ifndef WIN32 \n #define DEFAULT_HEAP_PATH \"" ++ installLib </> "csv%v/%m\""
    ++ "\n #endif"                                                                

  when disableCurses $ 
    liftIO $ appendFile (workArea </> "c/next_config.h") "define DISABLE_CURSES"

  return $ when disableX11 $ 
    appendFile (w </> "c/next_config.h") "define DISABLE_X11"


  liftIO $ renameFile (w </> "c/next_config.h") (w </> "c/config.h")

  let upupsrcdir = if isAbsolute srcdir then srcdir else "../.." </> srcdir

  let cFlags = if addFlags then cFlagsTmp ++ threadFlags else cFlagsTmp
      mdcppFlags = case m2 of
                     Just (Mach _ _ S2) -> ["-DSOlARIS"]
                     _ -> []
      ldFlags = ldFlags2
      libs = libs3
      mach = fromJust m2
      upupupbootdir="../../.." -- yep
      exePostStep = case flagsm of
                      Just (Mach _ _ NB) -> True
                      _ -> False
      installBin = installBin2
      installLib = installLib2
      installMan = installMan2
      archincludes = getArchincludes mach
      cppFlags = cppFlags2

  let m = mach
      
  return Config{..}

-- run command and on success do success otherwise do failure
f :: [String] -> Run b -> Run b -> Run b
f c success failure = do
  e <- withExitCode c
  case e of
    ExitSuccess -> success
    _ -> failure

-- similar but different to one below
getCodesDarwin :: Run (Maybe Mach, Maybe Mach, Maybe Mach, Maybe Mach)
getCodesDarwin = do
  e <- withExitCode ["uname", "-a", "|", "egrep", "\'i386|i686|amd64|athlon|x86_64\'", ">", "/dev/null", "2>&1"] -- same as below so could use the same 
  case e of
    ExitSuccess -> return (Just $ Mach False I3 OSX, Just $ Mach False A6 OSX, Just $ Mach True I3 OSX, Just $ Mach True A6 OSX)
    _ -> do
      e <- withExitCode ["uname", "-a", "|", "egrep", "\'arm|aarch\'", ">", "/dev/null", "2>&1"]
      case e of
        ExitSuccess -> return (Nothing, Just $ Mach False ARM64 OSX, Nothing, Just $ Mach True ARM64 OSX)
        _ -> do
          e <- withExitCode ["uname", "-a", "|", "egrep", "\'Power\'", ">", "/dev/null", "2>&1"]
          case e of
            ExitSuccess -> return (Nothing, Just $ Mach False PPC32 OSX, Nothing, Just $ Mach True PPC32 OSX) -- this seems like an error on the configure's part
            _ -> return (Nothing, Nothing, Nothing, Nothing)

withExitCode :: [String] -> Run ExitCode
withExitCode x = do
  let ef = show (hash $ unwords x) <.> "ef"
  cmd Shell $ x ++ [";", "echo", "$?", ">", ef]
  y <- liftIO $ readFile ef
  let c = read (trim y) in
    if c == 0
    then return ExitSuccess
    else return (ExitFailure c)
      
getCodes :: System -> Run (Maybe Mach, Maybe Mach, Maybe Mach, Maybe Mach)
getCodes unixSuffix = do
  e <- withExitCode ["uname", "-a", "|", "egrep", "\'i386|i686|amd64|athlon|x86_64\'", ">", "/dev/null", "2>&1"] 
  case e of
    ExitSuccess -> return (Just $ Mach False I3 unixSuffix, Just $ Mach False A6 unixSuffix, Just $ Mach True I3 unixSuffix, Just $ Mach True A6 unixSuffix)
    _ -> do
      e <- withExitCode ["uname", "-a", "|", "egrep", "\'power|ppc\'", ">", "/dev/null", "2>&1"]
      case e of
        ExitSuccess -> return (Just $ Mach False PPC32 unixSuffix, Nothing, Just $ Mach True PPC32 unixSuffix, Nothing)
        _ -> do
          e <- withExitCode ["uname", "-a", "|", "egrep", "\'armv|aarch64\'", ">", "/dev/null", "2>&1"]
          case e of
            ExitSuccess -> return (Just $ Mach False ARM32 unixSuffix, Just $ Mach False ARM64 unixSuffix, Just $ Mach True ARM32 unixSuffix, Just $ Mach True ARM64 unixSuffix)
            _ -> return (Nothing, Nothing, Nothing, Nothing)

submodInstructions :: String -> IO ()
submodInstructions msg = do
  putStrLn $ msg ++ "; check out Git submodules using"
  putStrLn "git submodule init"
  putStrLn "git submodule update"
  die ""

configure :: Run Config
configure = initConfig

semiConfigure :: FilePath -> Mach -> IO SC.Config
semiConfigure cd m = do
  let srcdir = "." -- Not sure how to get srcdir accurately here. and i know this works for our usecase
  let upupsrcdir = if isAbsolute srcdir then srcdir else "../.." </> srcdir
  let upupupsrcdir = if isAbsolute srcdir then srcdir else "../../.." </> srcdir
  upupupbootdir <- liftIO $ ifM (andM [S.doesFileExist $ cd </> "boot" </> showMach m </> "scheme.boot"
                                      ,(getFileStatus $ cd </> "boot" </> showMach m </> "scheme.boot") >>= pure . isRegularFile])
                   (pure "../../..")
                   (pure upupupsrcdir)
  let archincludes = getArchincludes m
  pure SC.Config{..}
  
