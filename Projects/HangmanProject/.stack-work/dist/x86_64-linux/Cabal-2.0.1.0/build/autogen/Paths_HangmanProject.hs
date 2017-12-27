{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_HangmanProject (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/datascience/projects/statisticallyfit/github/Haskell/HaskellTutorial/Projects/HangmanProject/.stack-work/install/x86_64-linux/lts-10.1/8.2.2/bin"
libdir     = "/datascience/projects/statisticallyfit/github/Haskell/HaskellTutorial/Projects/HangmanProject/.stack-work/install/x86_64-linux/lts-10.1/8.2.2/lib/x86_64-linux-ghc-8.2.2/HangmanProject-0.1.0.0-Izv0M6PDQ991in1cSnCpCP"
dynlibdir  = "/datascience/projects/statisticallyfit/github/Haskell/HaskellTutorial/Projects/HangmanProject/.stack-work/install/x86_64-linux/lts-10.1/8.2.2/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/datascience/projects/statisticallyfit/github/Haskell/HaskellTutorial/Projects/HangmanProject/.stack-work/install/x86_64-linux/lts-10.1/8.2.2/share/x86_64-linux-ghc-8.2.2/HangmanProject-0.1.0.0"
libexecdir = "/datascience/projects/statisticallyfit/github/Haskell/HaskellTutorial/Projects/HangmanProject/.stack-work/install/x86_64-linux/lts-10.1/8.2.2/libexec/x86_64-linux-ghc-8.2.2/HangmanProject-0.1.0.0"
sysconfdir = "/datascience/projects/statisticallyfit/github/Haskell/HaskellTutorial/Projects/HangmanProject/.stack-work/install/x86_64-linux/lts-10.1/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "HangmanProject_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HangmanProject_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "HangmanProject_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "HangmanProject_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HangmanProject_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HangmanProject_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
