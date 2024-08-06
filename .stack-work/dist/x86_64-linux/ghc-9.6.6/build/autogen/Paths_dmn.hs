{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_dmn (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [0,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/root/cclaw/dmn/.stack-work/install/x86_64-linux/02bd82326cf16573937b0e43a8e0cfed677538bf7de3c12a06a490b4feff1ff5/9.6.6/bin"
libdir     = "/root/cclaw/dmn/.stack-work/install/x86_64-linux/02bd82326cf16573937b0e43a8e0cfed677538bf7de3c12a06a490b4feff1ff5/9.6.6/lib/x86_64-linux-ghc-9.6.6/dmn-0.0.0-HXjXuOqE0bLHbrf8QqWDBL"
dynlibdir  = "/root/cclaw/dmn/.stack-work/install/x86_64-linux/02bd82326cf16573937b0e43a8e0cfed677538bf7de3c12a06a490b4feff1ff5/9.6.6/lib/x86_64-linux-ghc-9.6.6"
datadir    = "/root/cclaw/dmn/.stack-work/install/x86_64-linux/02bd82326cf16573937b0e43a8e0cfed677538bf7de3c12a06a490b4feff1ff5/9.6.6/share/x86_64-linux-ghc-9.6.6/dmn-0.0.0"
libexecdir = "/root/cclaw/dmn/.stack-work/install/x86_64-linux/02bd82326cf16573937b0e43a8e0cfed677538bf7de3c12a06a490b4feff1ff5/9.6.6/libexec/x86_64-linux-ghc-9.6.6/dmn-0.0.0"
sysconfdir = "/root/cclaw/dmn/.stack-work/install/x86_64-linux/02bd82326cf16573937b0e43a8e0cfed677538bf7de3c12a06a490b4feff1ff5/9.6.6/etc"

getBinDir     = catchIO (getEnv "dmn_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "dmn_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "dmn_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "dmn_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "dmn_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "dmn_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
