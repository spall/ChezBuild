
module C.Base(include, cpsrc
             ,kernelsrc, kernelLib, kernelObj,mainsrc,mainObj
             ,main, scheme, kernel, kernelO, pbchunksrc) where

import System.Directory
import System.FilePath
import System.Info.Extra
import System.FilePattern.Directory
import System.Posix.Files
import Control.Monad.Extra

import Development.Rattle

include m = "../boot" </> m -- </> "tmp"
petiteBoot m = "../boot" </> m </> "petite.boot"
schemeBoot m = "../boot" </> m </> "scheme.boot"
main m o = "../boot" </> m </> "main" <.> o
scheme m exeSuffix = "../bin" </> m </> "scheme" ++ exeSuffix

-- # One of these sets is referenced in Mf-config to select between
-- # linking with kernel.o or libkernel.a

kernelO m o = "../boot" </> m </> "kernel" <.> o
kernelOLinkDeps = ""
kernelOLinkLibs = ""

kernel = kernelLib
kernelLib m = "../boot" </> m </> "libkernel.a"

kernelsrc = ["statics.c", "segment.c", "alloc.c", "symbol.c", "intern.c", "gcwrapper.c", "gc-011.c", "gc-par.c", "gc-ocd.c"
            ,"gc-oce.c", "number.c", "schsig.c", "io.c", "new-io.c", "print.c", "fasl.c", "vfasl.c"
            ,"stats.c", "foreign.c", "prim.c", "prim5.c", "flushcache.c", "schlib.c", "thread.c"
            ,"expeditor.c", "scheme.c", "compress-io.c", "random.c", "ffi.c"]

-- IO [FileName]
pbchunksrc cd m = fmap (map takeFileName) $ getDirectoryFiles (cd </> "../boot" </> m) ["*.c"]

-- IO [FileName]
kernelObj cd m o mdobj = fmap ((mdobj ++ map (-<.> o) kernelsrc) ++) $ fmap (map (-<.> o)) $ pbchunksrc cd m

kernelHdr = ["system.h", "types.h", "version.h", "globals.h", "externs.h", "segment.h", "atomic.h", "gc.c"
            ,"sort.h", "thread.h", "config.h", "compress-io.h", "itest.c", "nocurses.h", "popcount.h", "pb.h"]

mainsrc = "main.c"

mainObj o = mainsrc -<.> o

cpsrc :: FilePath -> IO ()
cpsrc f = if isWindows
          then cmd ["cp", "-p", "../../c" </> f, f]
          else ifM (doesPathExist f)
               (return ())
               $ createSymbolicLink ("../../c" </> f) f

rootsrc = getDirectoryFiles "../../c"

lZ4Sources = ["../lz4/lib/lz4.h", "../lz4/lib/lz4frame.h", "../lz4/lib/lz4.c"
             ,"../lz4/lib/lz4frame.c", "../lz4/lib/lz4hc.c", "../lz4/lib/xxhash.c"]
