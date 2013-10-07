module Paths_HaskolliderIntro (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/octopian/.cabal/bin"
libdir     = "/home/octopian/.cabal/lib/HaskolliderIntro-0.1/ghc-7.6.3"
datadir    = "/home/octopian/.cabal/share/HaskolliderIntro-0.1"
libexecdir = "/home/octopian/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "HaskolliderIntro_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HaskolliderIntro_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "HaskolliderIntro_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HaskolliderIntro_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
