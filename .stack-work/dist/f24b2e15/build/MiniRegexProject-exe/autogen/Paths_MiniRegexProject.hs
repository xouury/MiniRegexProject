{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_MiniRegexProject (
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
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\Users\\lessi\\Desktop\\second_year\\summer_2024_2025\\Non-procedural programming\\MiniRegexProject\\.stack-work\\install\\8d7e95af\\bin"
libdir     = "C:\\Users\\lessi\\Desktop\\second_year\\summer_2024_2025\\Non-procedural programming\\MiniRegexProject\\.stack-work\\install\\8d7e95af\\lib\\x86_64-windows-ghc-9.8.4\\MiniRegexProject-0.1.0.0-4c3hOVOi0XD2M5DIxEK0CC-MiniRegexProject-exe"
dynlibdir  = "C:\\Users\\lessi\\Desktop\\second_year\\summer_2024_2025\\Non-procedural programming\\MiniRegexProject\\.stack-work\\install\\8d7e95af\\lib\\x86_64-windows-ghc-9.8.4"
datadir    = "C:\\Users\\lessi\\Desktop\\second_year\\summer_2024_2025\\Non-procedural programming\\MiniRegexProject\\.stack-work\\install\\8d7e95af\\share\\x86_64-windows-ghc-9.8.4\\MiniRegexProject-0.1.0.0"
libexecdir = "C:\\Users\\lessi\\Desktop\\second_year\\summer_2024_2025\\Non-procedural programming\\MiniRegexProject\\.stack-work\\install\\8d7e95af\\libexec\\x86_64-windows-ghc-9.8.4\\MiniRegexProject-0.1.0.0"
sysconfdir = "C:\\Users\\lessi\\Desktop\\second_year\\summer_2024_2025\\Non-procedural programming\\MiniRegexProject\\.stack-work\\install\\8d7e95af\\etc"

getBinDir     = catchIO (getEnv "MiniRegexProject_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "MiniRegexProject_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "MiniRegexProject_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "MiniRegexProject_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "MiniRegexProject_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "MiniRegexProject_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
