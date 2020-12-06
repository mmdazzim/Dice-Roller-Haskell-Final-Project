{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_haskellFinal (
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

bindir     = "C:\\Users\\mmdaz\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\mmdaz\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.10.2\\haskellFinal-0.1.0.0-inplace-haskellFinal"
dynlibdir  = "C:\\Users\\mmdaz\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.10.2"
datadir    = "C:\\Users\\mmdaz\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.10.2\\haskellFinal-0.1.0.0"
libexecdir = "C:\\Users\\mmdaz\\AppData\\Roaming\\cabal\\haskellFinal-0.1.0.0-inplace-haskellFinal\\x86_64-windows-ghc-8.10.2\\haskellFinal-0.1.0.0"
sysconfdir = "C:\\Users\\mmdaz\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskellFinal_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskellFinal_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "haskellFinal_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "haskellFinal_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskellFinal_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskellFinal_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
