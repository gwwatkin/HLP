{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_LinearProgramming (
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
version = Version [0,0,1,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/george/.cabal/bin"
libdir     = "/home/george/.cabal/lib/x86_64-linux-ghc-8.0.2/LinearProgramming-0.0.1.1-EzYmlxZk2yHBfI2hToMMnE"
dynlibdir  = "/home/george/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/george/.cabal/share/x86_64-linux-ghc-8.0.2/LinearProgramming-0.0.1.1"
libexecdir = "/home/george/.cabal/libexec"
sysconfdir = "/home/george/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "LinearProgramming_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "LinearProgramming_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "LinearProgramming_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "LinearProgramming_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "LinearProgramming_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "LinearProgramming_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
