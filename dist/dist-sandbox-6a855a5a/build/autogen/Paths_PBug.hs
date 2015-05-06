module Paths_PBug (
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
version = Version {versionBranch = [0,1,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/h/sprasa02/Desktop/PBug/.cabal-sandbox/bin"
libdir     = "/h/sprasa02/Desktop/PBug/.cabal-sandbox/lib/x86_64-linux-ghc-7.8.3/PBug-0.1.0.1"
datadir    = "/h/sprasa02/Desktop/PBug/.cabal-sandbox/share/x86_64-linux-ghc-7.8.3/PBug-0.1.0.1"
libexecdir = "/h/sprasa02/Desktop/PBug/.cabal-sandbox/libexec"
sysconfdir = "/h/sprasa02/Desktop/PBug/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "PBug_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "PBug_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "PBug_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "PBug_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "PBug_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
