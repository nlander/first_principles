module Paths_cipher (
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

bindir     = "/home/nathan/programmingFromFirstPrinciples/ch9/cipher/.stack-work/install/x86_64-linux/lts-3.7/7.10.2/bin"
libdir     = "/home/nathan/programmingFromFirstPrinciples/ch9/cipher/.stack-work/install/x86_64-linux/lts-3.7/7.10.2/lib/x86_64-linux-ghc-7.10.2/cipher-0.1.0.0-GPYUIE8kjCL55yn7PvZrUR"
datadir    = "/home/nathan/programmingFromFirstPrinciples/ch9/cipher/.stack-work/install/x86_64-linux/lts-3.7/7.10.2/share/x86_64-linux-ghc-7.10.2/cipher-0.1.0.0"
libexecdir = "/home/nathan/programmingFromFirstPrinciples/ch9/cipher/.stack-work/install/x86_64-linux/lts-3.7/7.10.2/libexec"
sysconfdir = "/home/nathan/programmingFromFirstPrinciples/ch9/cipher/.stack-work/install/x86_64-linux/lts-3.7/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cipher_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cipher_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "cipher_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cipher_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cipher_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
