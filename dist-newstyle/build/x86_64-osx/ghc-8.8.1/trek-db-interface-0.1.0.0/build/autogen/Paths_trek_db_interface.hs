{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_trek_db_interface (
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

bindir     = "/Users/jonathanfischoff/.cabal/bin"
libdir     = "/Users/jonathanfischoff/.cabal/lib/x86_64-osx-ghc-8.8.1/trek-db-interface-0.1.0.0-inplace"
dynlibdir  = "/Users/jonathanfischoff/.cabal/lib/x86_64-osx-ghc-8.8.1"
datadir    = "/Users/jonathanfischoff/.cabal/share/x86_64-osx-ghc-8.8.1/trek-db-interface-0.1.0.0"
libexecdir = "/Users/jonathanfischoff/.cabal/libexec/x86_64-osx-ghc-8.8.1/trek-db-interface-0.1.0.0"
sysconfdir = "/Users/jonathanfischoff/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "trek_db_interface_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "trek_db_interface_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "trek_db_interface_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "trek_db_interface_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "trek_db_interface_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "trek_db_interface_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
