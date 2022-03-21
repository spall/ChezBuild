
module S.Config(Config(..), BaseVars(..), defaultVars) where

import Mach
import System.Info.Extra
import System.FilePath

-- defaults of the variables in s/Mf-base
defaultVars m = BaseVars{o=3, d=0, cl="(commonization-level)", i=False, cp0=2, fc=True
                        ,xf="(compress-format)", xl="(compress-level)", p=False, xp=False
                        ,bp=False, xbp=False, c=False, loadspd=False, dumpspd=False
                        ,loadbpd=False, dumpbpd=False, compile="compile-file", pdhtml=False
                        ,gac=False, gic=False, pps=False, exeSuffix=if isWindows then ".exe" else ""
                        ,dirsep=if isWindows then ";" else ":", patchFile="", patch="patch"
                        ,scheme="../bin" </> showMach m </> "scheme" ++ if isWindows then ".exe" else ""
                        ,schemeHeapDirs=["../boot" </> showMach m], chezSchemeLibDirs=["."]
                        ,profileDumpSource="source.pd", profileDumpBlock="block.pd"}

data Config = Config
  {upupsrcdir :: FilePath
  ,upupupbootdir :: FilePath
  ,m :: Mach
  ,archincludes :: [String]
  }

-- variables defined in s/Mf-base
-- the reason for doing it this way is someone calling a function in S.Base.hs might want
-- to change an arbitrary amount of the default of these values.
-- defining them in a data type and then creating a default of that data type will
-- make it possible to do this. S.Config can just be passed to each function.
data BaseVars = BaseVars
  {o :: Integer -- o determines the optimize level
  ,d :: Integer -- d is the debug level at which the system should be built
  ,cl :: String -- cl determines the commonization level
  ,i :: Bool -- i determines whether inspector-information is generated
  ,cp0 :: Integer -- cp0 determines the number of cp0 (source optimizer) iterations run
  ,fc :: Bool -- fc determines whether fasl objects are compressed
  ,xf :: String -- xf determines the compression format
  ,xl :: String -- xl determine the compression level
  ,p :: Bool -- p determines whether source profiling is enabled
  ,xp :: Bool -- xp determines whether source profiling is enabled
  ,bp :: Bool -- bp determines whether binary profiling is enabled
  ,xbp :: Bool -- xpb determines whether binary profiling is enabled
  ,c :: Bool -- c determines whether covin files are generated
  ,loadspd :: Bool -- loadspd determines whether source-profile data is loaded
  ,dumpspd :: Bool -- dumpspd determines whether source-profile data is dumped
  ,loadbpd :: Bool -- loadbpd determines whether binary-profile data is loaded
  ,dumpbpd :: Bool -- dumpbpd determines whether binary-profile data is dumped
  ,compile :: String -- compile determines the entry point for compilng files
  ,pdhtml :: Bool -- pdhtml determines whether profile-dump-html is called at the end of a build
  ,gac :: Bool -- gac determines whether cost-center allocation counts are generated
  ,gic :: Bool -- gic determines whether cost-center instruction counts are generated
  ,pps :: Bool -- pps determines whether pass timings are printed
  ,exeSuffix :: String
  ,scheme :: FilePath
  ,schemeHeapDirs :: [FilePath]
  ,chezSchemeLibDirs :: [FilePath]
  ,dirsep :: String -- Define the libdirs separator character
  ,patchFile :: String
  ,patch :: String
  ,profileDumpSource :: FilePath
  ,profileDumpBlock :: FilePath
  }
