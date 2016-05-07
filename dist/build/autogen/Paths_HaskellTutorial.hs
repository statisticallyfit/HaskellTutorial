module Paths_HaskellTutorial (
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
version = Version [1,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/developer/.cabal/bin"
libdir     = "/home/developer/.cabal/lib/x86_64-linux-ghc-7.10.3/HaskellTutorial-1.0-748QWVQ1sr4DfSC9I27qfe"
datadir    = "/home/developer/.cabal/share/x86_64-linux-ghc-7.10.3/HaskellTutorial-1.0"
libexecdir = "/home/developer/.cabal/libexec"
sysconfdir = "/home/developer/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "HaskellTutorial_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HaskellTutorial_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "HaskellTutorial_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HaskellTutorial_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HaskellTutorial_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
