{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Addition (
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

bindir     = "/Users/michaelwebb/git/learning/haskell_programming/addition/.stack-work/install/x86_64-osx/8b58e2bd7719fcf701ecd14df1e85b59f89618fe9c1838582041721f520f2a75/8.6.5/bin"
libdir     = "/Users/michaelwebb/git/learning/haskell_programming/addition/.stack-work/install/x86_64-osx/8b58e2bd7719fcf701ecd14df1e85b59f89618fe9c1838582041721f520f2a75/8.6.5/lib/x86_64-osx-ghc-8.6.5/Addition-0.1.0.0-IySQOMIh1wRGOrvhBdjmdZ"
dynlibdir  = "/Users/michaelwebb/git/learning/haskell_programming/addition/.stack-work/install/x86_64-osx/8b58e2bd7719fcf701ecd14df1e85b59f89618fe9c1838582041721f520f2a75/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/michaelwebb/git/learning/haskell_programming/addition/.stack-work/install/x86_64-osx/8b58e2bd7719fcf701ecd14df1e85b59f89618fe9c1838582041721f520f2a75/8.6.5/share/x86_64-osx-ghc-8.6.5/Addition-0.1.0.0"
libexecdir = "/Users/michaelwebb/git/learning/haskell_programming/addition/.stack-work/install/x86_64-osx/8b58e2bd7719fcf701ecd14df1e85b59f89618fe9c1838582041721f520f2a75/8.6.5/libexec/x86_64-osx-ghc-8.6.5/Addition-0.1.0.0"
sysconfdir = "/Users/michaelwebb/git/learning/haskell_programming/addition/.stack-work/install/x86_64-osx/8b58e2bd7719fcf701ecd14df1e85b59f89618fe9c1838582041721f520f2a75/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Addition_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Addition_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Addition_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Addition_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Addition_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Addition_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
