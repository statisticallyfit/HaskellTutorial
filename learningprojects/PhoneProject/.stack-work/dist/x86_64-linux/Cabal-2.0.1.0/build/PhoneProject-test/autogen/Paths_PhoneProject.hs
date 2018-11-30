{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_PhoneProject (
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

bindir     = "/development/projects/statisticallyfit/github/learningprogramming/Haskell/Learning/HaskellTutorial/learningprojects/PhoneProject/.stack-work/install/x86_64-linux/lts-10.1/8.2.2/bin"
libdir     = "/development/projects/statisticallyfit/github/learningprogramming/Haskell/Learning/HaskellTutorial/learningprojects/PhoneProject/.stack-work/install/x86_64-linux/lts-10.1/8.2.2/lib/x86_64-linux-ghc-8.2.2/PhoneProject-0.1.0.0-JJHr6tyLM3p4XCbPHO6os8-PhoneProject-test"
dynlibdir  = "/development/projects/statisticallyfit/github/learningprogramming/Haskell/Learning/HaskellTutorial/learningprojects/PhoneProject/.stack-work/install/x86_64-linux/lts-10.1/8.2.2/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/development/projects/statisticallyfit/github/learningprogramming/Haskell/Learning/HaskellTutorial/learningprojects/PhoneProject/.stack-work/install/x86_64-linux/lts-10.1/8.2.2/share/x86_64-linux-ghc-8.2.2/PhoneProject-0.1.0.0"
libexecdir = "/development/projects/statisticallyfit/github/learningprogramming/Haskell/Learning/HaskellTutorial/learningprojects/PhoneProject/.stack-work/install/x86_64-linux/lts-10.1/8.2.2/libexec/x86_64-linux-ghc-8.2.2/PhoneProject-0.1.0.0"
sysconfdir = "/development/projects/statisticallyfit/github/learningprogramming/Haskell/Learning/HaskellTutorial/learningprojects/PhoneProject/.stack-work/install/x86_64-linux/lts-10.1/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "PhoneProject_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "PhoneProject_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "PhoneProject_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "PhoneProject_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "PhoneProject_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "PhoneProject_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
