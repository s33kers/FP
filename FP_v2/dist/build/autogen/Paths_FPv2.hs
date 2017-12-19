{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_FPv2 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\tadas\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\tadas\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.1\\FPv2-0.1.0.0-Apb67UextewLOsO2oYAuvE"
datadir    = "C:\\Users\\tadas\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.1\\FPv2-0.1.0.0"
libexecdir = "C:\\Users\\tadas\\AppData\\Roaming\\cabal\\FPv2-0.1.0.0-Apb67UextewLOsO2oYAuvE"
sysconfdir = "C:\\Users\\tadas\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "FPv2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "FPv2_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "FPv2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "FPv2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "FPv2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
