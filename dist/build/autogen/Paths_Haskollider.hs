module Paths_Haskollider (
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
version = Version {versionBranch = [0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/octopian/.cabal/bin"
libdir     = "/home/octopian/.cabal/lib/x86_64-linux-ghc-7.6.3/Haskollider-0.0.1"
datadir    = "/home/octopian/.cabal/share/x86_64-linux-ghc-7.6.3/Haskollider-0.0.1"
libexecdir = "/home/octopian/.cabal/libexec"
sysconfdir = "/home/octopian/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Haskollider_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Haskollider_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Haskollider_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Haskollider_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Haskollider_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
