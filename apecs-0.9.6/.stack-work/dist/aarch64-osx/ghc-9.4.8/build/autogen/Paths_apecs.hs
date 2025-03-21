{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_apecs (
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
version = Version [0,9,6] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/toby/KickerOrHorsemanship/.stack-work/install/aarch64-osx/73202a1b5151b3ddc43ad0381a5bd8bea2e987293438ec507632a94817aef3ec/9.4.8/bin"
libdir     = "/Users/toby/KickerOrHorsemanship/.stack-work/install/aarch64-osx/73202a1b5151b3ddc43ad0381a5bd8bea2e987293438ec507632a94817aef3ec/9.4.8/lib/aarch64-osx-ghc-9.4.8/apecs-0.9.6-LgtFLn511myKQvHkkVXJgT"
dynlibdir  = "/Users/toby/KickerOrHorsemanship/.stack-work/install/aarch64-osx/73202a1b5151b3ddc43ad0381a5bd8bea2e987293438ec507632a94817aef3ec/9.4.8/lib/aarch64-osx-ghc-9.4.8"
datadir    = "/Users/toby/KickerOrHorsemanship/.stack-work/install/aarch64-osx/73202a1b5151b3ddc43ad0381a5bd8bea2e987293438ec507632a94817aef3ec/9.4.8/share/aarch64-osx-ghc-9.4.8/apecs-0.9.6"
libexecdir = "/Users/toby/KickerOrHorsemanship/.stack-work/install/aarch64-osx/73202a1b5151b3ddc43ad0381a5bd8bea2e987293438ec507632a94817aef3ec/9.4.8/libexec/aarch64-osx-ghc-9.4.8/apecs-0.9.6"
sysconfdir = "/Users/toby/KickerOrHorsemanship/.stack-work/install/aarch64-osx/73202a1b5151b3ddc43ad0381a5bd8bea2e987293438ec507632a94817aef3ec/9.4.8/etc"

getBinDir     = catchIO (getEnv "apecs_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "apecs_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "apecs_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "apecs_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "apecs_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "apecs_sysconfdir") (\_ -> return sysconfdir)




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
