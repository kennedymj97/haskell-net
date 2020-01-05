{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_deep_haskell (
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

bindir     = "/mnt/c/Users/kenne/Developer/deep-haskell/backend/.stack-work/install/x86_64-linux/1cc6a27768558603cf2f963a4eb9fc1fef61d3c8a6bc29e46520397228832085/8.6.5/bin"
libdir     = "/mnt/c/Users/kenne/Developer/deep-haskell/backend/.stack-work/install/x86_64-linux/1cc6a27768558603cf2f963a4eb9fc1fef61d3c8a6bc29e46520397228832085/8.6.5/lib/x86_64-linux-ghc-8.6.5/deep-haskell-0.1.0.0-JJRcoZciXh3LMrmI5qpHWp-deep-haskell-exe"
dynlibdir  = "/mnt/c/Users/kenne/Developer/deep-haskell/backend/.stack-work/install/x86_64-linux/1cc6a27768558603cf2f963a4eb9fc1fef61d3c8a6bc29e46520397228832085/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/mnt/c/Users/kenne/Developer/deep-haskell/backend/.stack-work/install/x86_64-linux/1cc6a27768558603cf2f963a4eb9fc1fef61d3c8a6bc29e46520397228832085/8.6.5/share/x86_64-linux-ghc-8.6.5/deep-haskell-0.1.0.0"
libexecdir = "/mnt/c/Users/kenne/Developer/deep-haskell/backend/.stack-work/install/x86_64-linux/1cc6a27768558603cf2f963a4eb9fc1fef61d3c8a6bc29e46520397228832085/8.6.5/libexec/x86_64-linux-ghc-8.6.5/deep-haskell-0.1.0.0"
sysconfdir = "/mnt/c/Users/kenne/Developer/deep-haskell/backend/.stack-work/install/x86_64-linux/1cc6a27768558603cf2f963a4eb9fc1fef61d3c8a6bc29e46520397228832085/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "deep_haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "deep_haskell_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "deep_haskell_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "deep_haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "deep_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "deep_haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
