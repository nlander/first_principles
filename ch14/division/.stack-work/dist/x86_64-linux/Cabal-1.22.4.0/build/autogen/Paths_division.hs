module Paths_division (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/nathan/programmingFromFirstPrinciples/ch14/division/.stack-work/install/x86_64-linux/lts-3.7/7.10.2/bin"
libdir     = "/home/nathan/programmingFromFirstPrinciples/ch14/division/.stack-work/install/x86_64-linux/lts-3.7/7.10.2/lib/x86_64-linux-ghc-7.10.2/division-0.1.0.0-383lyAwimmM8tDCaie5nHe"
datadir    = "/home/nathan/programmingFromFirstPrinciples/ch14/division/.stack-work/install/x86_64-linux/lts-3.7/7.10.2/share/x86_64-linux-ghc-7.10.2/division-0.1.0.0"
libexecdir = "/home/nathan/programmingFromFirstPrinciples/ch14/division/.stack-work/install/x86_64-linux/lts-3.7/7.10.2/libexec"
sysconfdir = "/home/nathan/programmingFromFirstPrinciples/ch14/division/.stack-work/install/x86_64-linux/lts-3.7/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "division_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "division_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "division_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "division_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "division_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
