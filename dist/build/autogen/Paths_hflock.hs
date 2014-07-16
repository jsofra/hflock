module Paths_hflock (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/jsofra/.cabal/bin"
libdir     = "/home/jsofra/.cabal/lib/hflock-0.1.0.0/ghc-7.6.3"
datadir    = "/home/jsofra/.cabal/share/hflock-0.1.0.0"
libexecdir = "/home/jsofra/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "hflock_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hflock_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hflock_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hflock_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
