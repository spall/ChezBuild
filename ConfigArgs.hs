{-# LANGUAGE RecordWildCards #-}

module ConfigArgs where

import System.Console.CmdArgs.Explicit
import Mach

type Variable = String

data ConfigArgs = ConfigArgs
  {m :: Maybe Mach -- x
  ,pb :: Bool -- x
  ,pbarch :: Bool
  ,libs :: [String] -- x
  ,workArea :: String -- x
  ,threads :: Bool -- x
  ,tempRoot :: String -- x
  ,installPrefix :: String -- x
  ,installOwner :: String -- x
  ,installGroup :: String -- x
  ,installBin :: String -- x
  ,installLib :: String -- x
  ,installMan :: String -- x
  ,installSchemeName :: String -- x
  ,installPetiteName :: String -- x
  ,installScriptName :: String -- x
  ,gzipManPages :: Bool -- x
  ,disableX11 :: Bool -- x
  ,disableCurses :: Bool -- x
  ,disableIConv :: Bool -- x
  ,addFlags :: Bool -- x
  ,enableWarningFlags :: Bool -- x
  ,cc :: String -- x
  ,cppFlags :: [String] -- x
  ,cFlags :: [String] -- x
  ,cFlagsSet :: Bool -- x
  ,ld :: String -- x
  ,ldFlags :: [String] -- x
  ,ar :: String -- x
  ,arFlags :: [String] -- x
  ,ranlib :: String -- x 
  ,windres :: String -- x
  ,zlibInc :: FilePath -- x
  ,lz4Inc :: FilePath -- x
  ,zlibDep :: FilePath -- x
  ,lz4Dep :: FilePath -- x
  ,zlibLib :: FilePath -- x
  ,lz4Lib :: FilePath -- x
  ,zLibHeaderDep :: [FilePath] -- x
  ,lz4HeaderDep :: [FilePath] -- x
  ,kernel :: String -- x
  ,installzLibTarget :: Bool -- x
  ,installlz4Target :: Bool -- x
  ,bits :: BITS -- x
  ,libffi :: Bool
  ,emscripten :: Bool
  ,emBootFiles :: [String]
  ,empetite :: Bool
  ,mboot :: String
  ,alwaysUseBootFile :: String
  ,forceWorkArea :: Bool
  }

  -----------------------------------------------------------------------------------------

  -- the flags
forceFlag :: Flag ConfigArgs
forceFlag = flagNone ["force"] update "configure even without boot files"
  where update config = config{forceWorkArea=True}
  
bootFlag :: Flag ConfigArgs
bootFlag = flagReq ["boot"] update "<machine type>-<tag>" "build from prepared variant (e.g., pbchunk)"
  where update val config = Right config{mboot=val}

startFlag :: Flag ConfigArgs
startFlag = flagReq ["start"] update "<name>" "load <boot>.boot instead of <exe>.boot"
  where update val config = Right config{alwaysUseBootFile=val}
  
emscriptenFlag :: Flag ConfigArgs
emscriptenFlag = flagNone ["emscripten"] update "build via emscripten (\"em\" tool prefix)"
  where update config = config{emscripten = True, cc = "emcc", ld = "emld", ar = "emar"
                              , ranlib = "emranlib", pb = True, bits = BITS32
                              , disableIConv = True, disableCurses = True}

embootFlag :: Flag ConfigArgs
embootFlag = flagReq ["emboot"] update "boot files" "additional boot <file>s with emscripten"
  where update val config = Right config{emBootFiles=(emBootFiles config) ++ words val}

empetiteFlag :: Flag ConfigArgs
empetiteFlag = flagNone ["empetite"] update "omit \"scheme.boot\" with emscripten"
  where update config = config{empetite = True}
  
libffiFlag :: Flag ConfigArgs
libffiFlag = flagNone ["enable-libffi"] update "enable libffi support for pb"
  where update config = config{libffi = True}
  
mFlag :: [String] -> Flag ConfigArgs
mFlag machs = flagReq ["m", "machine"] update "machine type" "explicitly specify machine type"
  where update val config = if elem val machs
                            then Right config{m = Just (read val)}
                            else Left $ "no suitable machine type found; available machine types: " ++ show machs

threadsFlag :: Flag ConfigArgs
threadsFlag = flagNone ["threads"] update "specify threaded version"
  where update config = config{threads = True}

noThreadsFlag :: Flag ConfigArgs
noThreadsFlag = flagNone ["nothreads"] update "specify no threaded version"
  where update config = config{threads = False}

bitsFlag :: Flag ConfigArgs
bitsFlag = flagReq ["bits"] update "64 | 32" "specify 32/64-bit version"
  where update "64" config = Right config{bits=BITS64}
        update "32" config = Right config{bits=BITS32}
        update _ config = Left "choose either 32 or 64 bits"

pbarchFlag :: Flag ConfigArgs
pbarchFlag = flagNone ["pbarch"] update "specify pb with host word and endianness"
  where update config = config{pbarch = True, pb = True, threads = True}

pbFlag :: Flag ConfigArgs
pbFlag = flagNone ["pb"] update "specify pb (portable bytecode) version"
  where update config = config{pb = True, threads = False}

installPrefixFlag :: Flag ConfigArgs
installPrefixFlag = flagReq ["installprefix"] update "pathname" "final installation root"
  where update v config = Right config{installPrefix=v}

installBinFlag :: Flag ConfigArgs
installBinFlag = flagReq ["installbin"] update "pathname" "bin directory"
  where update v config = Right config{installBin=v}

installLibFlag :: Flag ConfigArgs
installLibFlag = flagReq ["installlib"] update "pathname" "lib directory"
  where update v config = Right config{installLib=v}

installManFlag :: Flag ConfigArgs
installManFlag = flagReq ["installman"] update "pathname" "manpage directory"
  where update v config = Right config{installMan=v}

installOwnerFlag :: Flag ConfigArgs
installOwnerFlag = flagReq ["installowner"] update "ownername" "install with owner"
  where update v config = Right config{installOwner=v}

installGroupFlag :: Flag ConfigArgs
installGroupFlag = flagReq ["installgroup"] update "groupname" "install with group"
  where update v config = Right config{installGroup=v}

installSchemeNameFlag :: Flag ConfigArgs
installSchemeNameFlag = flagReq ["installschemename"] update "schemename" "install with group"
  where update v config = Right config{installSchemeName=v}

installPetiteNameFlag :: Flag ConfigArgs
installPetiteNameFlag = flagReq ["installpetitename"] update "petitename" "install with group"
  where update v config = Right config{installPetiteName=v}

installScriptNameFlag :: Flag ConfigArgs
installScriptNameFlag = flagReq ["installscriptname"] update "scriptname" "install with group"
  where update v config = Right config{installScriptName=v}

toolPrefixFlag :: Flag ConfigArgs
toolPrefixFlag = flagReq ["toolprefix"] update "prefix" "prefix tool (compiler, linker, ...) names"
  where update v config@ConfigArgs{..} = Right config{cc=v ++ cc, ld = v ++ ld, ar = v ++ ar
                                                 ,ranlib = v ++ ranlib, windres = v ++ windres}

gzipManPagesFlag :: Flag ConfigArgs
gzipManPagesFlag = flagReq ["gzip-man-pages"] update "yes | no" "compress manual pages"
  where update "no" config = Right config{gzipManPages = False}
        update "yes" config = Right config{gzipManPages = True}
        update _ config = Left "expected yes or no"

tempRootFlag :: Flag ConfigArgs
tempRootFlag = flagReq ["temproot"] update "pathname" "staging root"
  where update v config = Right config{tempRoot=v}

workAreaFlag :: Flag ConfigArgs
workAreaFlag = flagReq ["workarea"] update "pathname" "build directory"
  where update v config = Right config{workArea=v}

disableX11Flag :: Flag ConfigArgs
disableX11Flag = flagNone ["disable-x11"] update "disable X11 support"
  where update config = config{disableX11 = True}

disableCursesFlag :: Flag ConfigArgs
disableCursesFlag = flagNone ["disable-curses"] update "disable [n]curses support"
  where update config = config{disableCurses = True}

disableIConvFlag :: Flag ConfigArgs
disableIConvFlag = flagNone ["disable-iconv"] update "disable iconv support"
  where update config = config{disableIConv = True}

disableAutoFlagsFlag :: Flag ConfigArgs
disableAutoFlagsFlag = flagNone ["disable-auto-flags"] update "no auto additions to CFLAGS/LDFLAGS/LIBS"
  where update config = config{addFlags = False}

enableWarningFlagsFlag :: Flag ConfigArgs
enableWarningFlagsFlag = flagNone ["enable-warning-flags"] update "add GCC warning flags to CFLAGS"
  where update config = config{enableWarningFlags = True}
  
libKernelFlag :: Flag ConfigArgs
libKernelFlag = flagNone ["libkernel"] update "build libkernel.a (the default)"
  where update config@ConfigArgs{..} =
          config{kernel="kernelLib", installzLibTarget = zlibInc /= ""
                ,installlz4Target = lz4Inc /= ""}

kernelObjFlag :: Flag ConfigArgs
kernelObjFlag = flagNone ["kernelobj"] update "build kernel.o instead of libkernel.a"
  where update config = config{kernel="kernelO",installzLibTarget=False, installlz4Target=False}

ccFlag :: Flag ConfigArgs
ccFlag = flagReq ["CC"] update "C compiler" "C compiler"
  where update v config = Right config{cc=v}

cppFlagsFlag :: Flag ConfigArgs
cppFlagsFlag = flagReq ["CPPFLAGS"] update "C preprocessor flags" "additional C preprocessor flags"
  where update v config = Right config{cppFlags=words v}

cFlagsFlag :: Flag ConfigArgs
cFlagsFlag = flagReq ["CFLAGS"] update "C compiler flags" "additional C compiler flags"
  where update v config = Right config{cFlags=words v, cFlagsSet = True}

ldFlag :: Flag ConfigArgs
ldFlag = flagReq ["LD"] update "linker" "linker"
  where update v config = Right config{ld=v}

ldFlagsFlag :: Flag ConfigArgs
ldFlagsFlag = flagReq ["LDFLAGS"] update "linker flags" "additional linker flags"
  where update v config = Right config{ldFlags=words v}

libsFlag :: Flag ConfigArgs
libsFlag = flagReq ["LIBS"] update "additional libraries" "additional libraries"
  where update v config = Right config{libs=words v}

arFlag :: Flag ConfigArgs
arFlag = flagReq ["AR"] update "archiver" "archiver"
  where update v config = Right config{ar=v}

arFlagsFlag :: Flag ConfigArgs
arFlagsFlag = flagReq ["ARFLAGS"] update "archiver flags" "archiver flags"
  where update v config = Right config{arFlags=words v}

ranLibFlag :: Flag ConfigArgs
ranLibFlag = flagReq ["RANLIB"] update "archive indexer" "archive indexer"
  where update v config = Right config{ranlib=v}

windresFlag :: Flag ConfigArgs
windresFlag = flagReq ["WINDRES"] update "resource compiler" "resource compiler"
  where update v config = Right config{windres=v}

zLibFlag :: Flag ConfigArgs
zLibFlag = flagReq ["ZLIB"] update "lib" "link to <lib> instead of own zlib"
  where update v config = Right config{zlibLib=v, zlibInc="", zlibDep="", zLibHeaderDep=[]
                                      ,installzLibTarget=False}

lz4Flag :: Flag ConfigArgs
lz4Flag = flagReq ["LZ4"] update "lib" "link to <lib> instead of own LZ4"
  where update v config = Right config{lz4Lib=v, lz4Inc="", lz4Dep="", lz4HeaderDep=[]
                                      ,installlz4Target=False}

-- TODO: rename this it sounds like a variable not a function
cargs :: [String] -> ConfigArgs -> Mode ConfigArgs
cargs machs ic = (modeEmpty ic)
           {modeNames=["explicit"], modeHelp= "help"
           ,modeGroupFlags = toGroup [mFlag machs, pbFlag, libsFlag, threadsFlag, bitsFlag, installPrefixFlag
                                     ,installBinFlag, installLibFlag, installManFlag
                                     ,installOwnerFlag, installGroupFlag, installSchemeNameFlag
                                     ,installPetiteNameFlag, installScriptNameFlag, toolPrefixFlag
                                     ,gzipManPagesFlag, tempRootFlag, workAreaFlag, disableX11Flag
                                     ,disableCursesFlag, disableIConvFlag, disableAutoFlagsFlag, enableWarningFlagsFlag, libKernelFlag, kernelObjFlag, ccFlag
                                     ,cppFlagsFlag, cFlagsFlag, ldFlag, ldFlagsFlag, arFlag
                                     ,arFlagsFlag, ranLibFlag, windresFlag, zLibFlag, lz4Flag
                                     ,flagHelpSimple id, libffiFlag, emscriptenFlag, embootFlag, empetiteFlag, bootFlag, forceFlag]}
