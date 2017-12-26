{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_luhn (
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

bindir     = "/datascience/projects/statisticallyfit/github/Haskell/HaskellTutorial/Projects/luhn/.stack-work/install/x86_64-linux/lts-10.0/8.2.2/bin"
libdir     = "/datascience/projects/statisticallyfit/github/Haskell/HaskellTutorial/Projects/luhn/.stack-work/install/x86_64-linux/lts-10.0/8.2.2/lib/x86_64-linux-ghc-8.2.2/luhn-0.1.0.0-2nf7XYv3GR5CyBF0Juaia0-luhn"
dynlibdir  = "/datascience/projects/statisticallyfit/github/Haskell/HaskellTutorial/Projects/luhn/.stack-work/install/x86_64-linux/lts-10.0/8.2.2/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/datascience/projects/statisticallyfit/github/Haskell/HaskellTutorial/Projects/luhn/.stack-work/install/x86_64-linux/lts-10.0/8.2.2/share/x86_64-linux-ghc-8.2.2/luhn-0.1.0.0"
libexecdir = "/datascience/projects/statisticallyfit/github/Haskell/HaskellTutorial/Projects/luhn/.stack-work/install/x86_64-linux/lts-10.0/8.2.2/libexec/x86_64-linux-ghc-8.2.2/luhn-0.1.0.0"
sysconfdir = "/datascience/projects/statisticallyfit/github/Haskell/HaskellTutorial/Projects/luhn/.stack-work/install/x86_64-linux/lts-10.0/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "luhn_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "luhn_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "luhn_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "luhn_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "luhn_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "luhn_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
