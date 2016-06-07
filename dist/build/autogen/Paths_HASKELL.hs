module Paths_HASKELL (
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
version = Version {versionBranch = [1,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/tony/Library/Haskell/bin"
libdir     = "/Users/tony/Library/Haskell/ghc-7.8.3-x86_64/lib/HASKELL-1.0"
datadir    = "/Users/tony/Library/Haskell/share/ghc-7.8.3-x86_64/HASKELL-1.0"
libexecdir = "/Users/tony/Library/Haskell/libexec"
sysconfdir = "/Users/tony/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "HASKELL_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HASKELL_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "HASKELL_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HASKELL_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HASKELL_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
