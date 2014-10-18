module Paths_creis (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/thejanet/dev/haskell/creis/.cabal-sandbox/bin"
libdir     = "/home/thejanet/dev/haskell/creis/.cabal-sandbox/lib/i386-linux-ghc-7.8.3/creis-0.1.0.0"
datadir    = "/home/thejanet/dev/haskell/creis/.cabal-sandbox/share/i386-linux-ghc-7.8.3/creis-0.1.0.0"
libexecdir = "/home/thejanet/dev/haskell/creis/.cabal-sandbox/libexec"
sysconfdir = "/home/thejanet/dev/haskell/creis/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "creis_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "creis_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "creis_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "creis_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "creis_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
