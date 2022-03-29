{-# Language ScopedTypeVariables #-}

module Mach where

import Data.Char
import Text.Read hiding (choice, look)
import Text.Parsec
import qualified Debug.Trace as T
        
data BITS = BITS64 | BITS32 deriving (Eq, Show, Read)

data Endian = Little | Big deriving (Eq, Show, Read)

data CPU = PORTABLE_BYTECODE | X86_64 | I386 | ARMV6 | AARCH64 | CPU_PPC32 deriving (Show, Read)

-- supported operating systems codes
data System = LE | NT | OSX | S2 | FB | OB | NB | QNX deriving (Eq, Show, Read)

-- supported hardware platform codes
data Platform = I3 | A6 | ARM32 | ARM64 | PPC32 deriving (Eq, Show, Read)

data Mach = Mach { threaded :: Bool -- is the system threaded or not
                 , platform :: Platform -- the hardware platform
                 , system :: System } -- the operating system
          | PB { threaded :: Bool -- is the system threaded or not
               , word :: Maybe BITS -- 64, 32 or Nothing for basic
               , endian :: Maybe Endian -- endianness; little, big or nothing for basic
               }
          | EM deriving (Eq, Show, Read)

showMach :: Mach -> String
showMach (PB t w e) = (f t) ++ "pb" ++ (g w) ++ (h e)
  where f True = "t"
        f False = ""
        g (Just BITS64) = "64"
        g (Just BITS32) = "32"
        g _ = ""
        h (Just Little) = "l"
        h (Just Big) = "b"
        h _ = ""
showMach (Mach True p s) = "t" ++ (map toLower $ show p) ++ (map toLower $ show s)
showMach (Mach False p s) = (map toLower $ show p) ++ (map toLower $ show s)

parseMach :: Parsec String () Mach
parseMach = try pTPB <|> try pFPB <|> pT <|> pF
    where pWord = optionMaybe $ choice [string "64" *> pure BITS64, string "32" *> pure BITS32]
          pEnd = optionMaybe $ choice [string "l" *> pure Little, string "b" *> pure Big]
          pTPB = do
            string "tpb"
            w <- pWord
            e <- pEnd
            eof
            pure $ PB True w e
          pFPB = do
            string "pb"
            w <- pWord
            e <- pEnd
            eof
            pure $ PB False w e
          pPlat = choice [string "i3" *> pure I3, string "a6" *> pure A6
                         ,string "arm32" *> pure ARM32, string "arm64" *> pure ARM64
                         ,string "ppc32" *> pure PPC32]
          pSys = choice [string "le" *> pure LE, string "nt" *> pure NT
                        ,string "osx" *> pure OSX, string "s2" *> pure S2
                        ,string "fb" *> pure FB, string "ob" *> pure OB
                        ,string "nb" *> pure NB, string "qnx" *> pure QNX]
          pT = do
            char 't'
            plat <- pPlat
            sys  <- pSys
            eof
            pure $ Mach True plat sys
          pF = do
            plat <- pPlat
            sys  <- pSys
            eof
            pure $ Mach False plat sys

readMach :: String -> IO Mach
readMach str = either (fail . show) pure $ parse parseMach "Mach.hs" str

testReadMach :: IO Mach
testReadMach = do
  m1 <- readMach "pb"
  m2 <- readMach "ta6le"
  m2 <- readMach "arm32osx"
  pure m1
      
------------------------- functions for getting flags --------------------------------------

getCpu :: Mach -> CPU
getCpu (PB _ _ _) = PORTABLE_BYTECODE
getCpu (Mach _ A6 _) = X86_64
getCpu (Mach _ I3 _) = I386
getCpu (Mach _ ARM32 _) = ARMV6
getCpu (Mach _ ARM64 _) = AARCH64
getCpu (Mach _ PPC32 _) = CPU_PPC32

-- returns thread flags and thread lib arguments
getThreadFlags :: Mach -> [String]
getThreadFlags EM = ["-pthread"]
getThreadFlags (Mach _ _ LE) = ["-D_REENTRANT", "-pthread"]
getThreadFlags (Mach _ _ FB) = ["-D_REENTRANT", "-pthread"]
getThreadFlags (Mach _ _ OB) = ["-D_REENTRANT", "-pthread"]
getThreadFlags (Mach _ _ NB) = ["-D_REENTRANT", "-pthread"]
getThreadFlags (Mach _ _ S2) = ["-pthread"]
getThreadFlags _ = []

getThreadLibs :: Mach -> [String]
getThreadLibs (Mach _ _ QNX) = []
getThreadLibs (Mach _ _ OSX) = []
getThreadLibs (Mach _ _ NT) = []
getThreadLibs (Mach _ _  _) = ["-lpthread"]
getThreadLibs _ = []

getCFlags :: [String] -> Mach -> [String]
getCFlags optFlags EM = optFlags
getCFlags optFlags (Mach _ A6 LE) = ["-m64", "-msse2"] ++ optFlags
getCFlags optFlags (Mach _ A6 NT) = optFlags
getCFlags optFlags (Mach _ A6 _) = ["-m64"] ++ optFlags
getCFlags optFlags (Mach _ I3 LE) = ["-m32", "-msse2", "-mfpmath=sse"] ++ optFlags
getCFlags optFlags (Mach _ I3 NT) = optFlags
getCFlags optFlags (Mach _ I3 QNX) = ["-m32", "-N2048K"] ++ optFlags
getCFlags optFlags (Mach _ I3 _) = ["-m32"] ++ optFlags
getCFlags optFlags (Mach _ ARM32 _) = optFlags
getCFlags optFlags (Mach _ ARM64 OSX) = ["-arch", "arm64"] ++ optFlags
getCFlags optFlags (Mach _ PPC32 OSX) = optFlags
getCFlags optFlags (Mach _ PPC32 _) = ["-m32"] ++ optFlags
getCFlags _ _ = []

getldFlags :: Mach -> [String]
getldFlags (Mach _ _ LE) = ["-rdynamic"]
getldFlags (Mach _ _ FB) = ["-rdynamic", "-L/usr/local/lib"]
getldFlags (Mach _ _ NB) = ["-rdynamic", "-L/usr/local/lib"]
getldFlags (Mach _ _ OB) = ["-rdynamic", "-Wl,--export-dynamic", "-Wl,-zwxneeded", "-L/usr/local/lib"]
getldFlags _ = []

getLibsFlags :: String -> String -> String -> Bool -> Mach -> [String]
getLibsFlags _ ncursesLib _ _ (Mach _ _ LE) = ["-lm", "-ldl", ncursesLib, "-lrt"]
getLibsFlags iconvLib ncursesLib _ _ (Mach _ _ FB) = [iconvLib , "-lm", ncursesLib]
getLibsFlags iconvLib ncursesLib _ _ (Mach _ _ OB) = [iconvLib , "-lm", ncursesLib]
getLibsFlags _ _ _ False (Mach _ _ NB) = ["/usr/lib/i18n/libiconv_std.a", "-lm", "/usr/pkg/lib/libncurses.a"]
getLibsFlags iconvLib _ _ True (Mach _ _ NB) = [iconvLib, "-lm", "/usr/pkg/lib/libncurses.a"]
getLibsFlags _ _ cursesLib _ (Mach _ _ S2) = ["-lnsl", "-ldl", "-lm", cursesLib, "-lrt"]
getLibsFlags iconvLib ncursesLib _ _ (Mach _ _ OSX) = [iconvLib , "-lm", ncursesLib]
getLibsFlags _ _ _ _ (Mach _ _ NT) = ["-lshell32", "-luser32", "-lole32", "-lrpcrt4", "-luuid"]
-- there is an 8qnx but dunno what that is and have left it out
getLibsFlags _ ncursesLib _ False (Mach _ _ QNX) = ["-lm", "/usr/local/lib/libiconv.so", "-lsocket", ncursesLib]
getLibsFlags iconvLib ncursesLib _ True (Mach _ _ QNX) = ["-lm", iconvLib, "-lsocket", ncursesLib]
getLibsFlags _ _ _ _ _ = []

getmdcFlags :: Mach -> [String]
getmdcFlags (Mach _ _ NT) = []
getmdcFlags (Mach _ _ OSX) = ["-dynamiclib", "-undefined", "dynamic_lookup"]
getmdcFlags (Mach _ _ _) = ["-fPIC", "-shared"]
getmdcFlags _ = []

getmdIncludes :: Mach -> [String]
getmdIncludes (Mach _ _ FB) = ["-I/usr/local/include", "-I/usr/X11R6/include"]
getmdIncludes (Mach _ _ OB) = ["-I/usr/local/include", "-I/usr/X11R6/include"]
getmdIncludes (Mach _ _ NB) = ["-I/usr/X11R7/include", "-I/usr/pkg/include", "-I/usr/pkg/include/ncurses", "-I/usr/X11R6/include"]
getmdIncludes (Mach _ _ QNX) = ["-I/usr/local/include"]
getmdIncludes _ = []

getmdldFlags :: Mach -> [String]
getmdldFlags (Mach _ A6 LE) = ["-melf_x86_64"]
getmdldFlags (Mach _ I3 LE) = ["-melf_i386"]
getmdldFlags (Mach _ I3 NB) = ["-m elf_i386"]
getmdldFlags (Mach _ A6 S2) = ["-melf_x86_64"]
getmdldFlags (Mach _ I3 S2) = ["-melf_i386"]
getmdldFlags (Mach _ I3 QNX) = ["-mi386nto"]
getmdldFlags _ = []

getzlibConfigFlags :: Mach -> [String]
getzlibConfigFlags _ = []

getArchincludes :: Mach -> [String]
getArchincludes (Mach _ A6 _) = ["x86_64.ss"]
getArchincludes (Mach _ ARM32 _) = ["arm32.ss"]
getArchincludes (Mach _ ARM64 _) = ["arm64.ss"]
getArchincludes (Mach _ I3 _) = ["x86.ss"]
getArchincludes (Mach _ PPC32 _) = ["ppc32.ss"]
getArchincludes (PB _ _ _) = ["pb.ss"]
getArchincludes _ = []
