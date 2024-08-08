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
bindir     = "/home/andres/repos/well-typed/cclaw/dmn/.stack-work/install/x86_64-linux/fa7df3be2bca71a063db05b7e8a7ffd02cfd8fca4c0f98416b21ceaad9b9b988/9.6.6/bin"
libdir     = "/home/andres/repos/well-typed/cclaw/dmn/.stack-work/install/x86_64-linux/fa7df3be2bca71a063db05b7e8a7ffd02cfd8fca4c0f98416b21ceaad9b9b988/9.6.6/lib/x86_64-linux-ghc-9.6.6/dmn-0.0.0-3QdOCqBJfQmG7l2dthX75l"
dynlibdir  = "/home/andres/repos/well-typed/cclaw/dmn/.stack-work/install/x86_64-linux/fa7df3be2bca71a063db05b7e8a7ffd02cfd8fca4c0f98416b21ceaad9b9b988/9.6.6/lib/x86_64-linux-ghc-9.6.6"
datadir    = "/home/andres/repos/well-typed/cclaw/dmn/.stack-work/install/x86_64-linux/fa7df3be2bca71a063db05b7e8a7ffd02cfd8fca4c0f98416b21ceaad9b9b988/9.6.6/share/x86_64-linux-ghc-9.6.6/dmn-0.0.0"
libexecdir = "/home/andres/repos/well-typed/cclaw/dmn/.stack-work/install/x86_64-linux/fa7df3be2bca71a063db05b7e8a7ffd02cfd8fca4c0f98416b21ceaad9b9b988/9.6.6/libexec/x86_64-linux-ghc-9.6.6/dmn-0.0.0"
sysconfdir = "/home/andres/repos/well-typed/cclaw/dmn/.stack-work/install/x86_64-linux/fa7df3be2bca71a063db05b7e8a7ffd02cfd8fca4c0f98416b21ceaad9b9b988/9.6.6/etc"

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
