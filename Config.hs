
module Config where

import Mach

data Config = Config
  {upupsrcdir :: FilePath
  ,upupupbootdir :: FilePath
  ,archincludes :: [String]
  ,workArea :: String
  ,m :: Mach
  ,srcdir :: FilePath
  ,cpu :: CPU
  ,mdarchsrc :: String
  ,mdinclude :: [String]
  ,mdcppFlags :: [String]
  ,mdldFlags :: [String]
  ,mdlinkFlags :: [String]
  ,exePostStep :: Bool
  ,zlibConfigureFlags :: [String]
  ,cc :: String
  ,cppFlags :: [String]
  ,cFlags :: [String]
  ,ld :: String
  ,ldFlags :: [String]
  ,libs :: [String]
  ,ar :: String
  ,arFlags :: [String]
  ,ranlib :: String
  ,windres :: String
  ,cursesLib :: String
  ,ncursesLib :: String
  ,zlibInc :: String
  ,lz4Inc :: String
  ,zlibDep :: String
  ,lz4Dep :: String
  ,zlibLib :: String
  ,lz4Lib :: String
  ,zLibHeaderDep :: [String]
  ,lz4HeaderDep :: [String]
  ,warningFlags :: [String]
  ,kernel :: String
  ,mdcFlags :: [String]
  -- for installation
  ,installBin :: String
  ,installLib :: String
  ,installMan :: String
  ,installOwner :: String
  ,installGroup :: String
  ,tempRoot :: String
  ,gzipManPages :: Bool
  ,installSchemeName :: String
  ,installPetiteName :: String
  ,installScriptName :: String
  ,installzLibTarget :: Bool
  ,installlz4Target :: Bool
  } deriving (Show, Read)
