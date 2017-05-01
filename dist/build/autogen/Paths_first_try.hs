module Paths_first_try (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/media/meir/CC18F3AB18F392A6/haskel/first try/.cabal-sandbox/bin"
libdir     = "/media/meir/CC18F3AB18F392A6/haskel/first try/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.3/first-try-0.1.0.0-3AE085yX5TvDCIfCoH7mck"
datadir    = "/media/meir/CC18F3AB18F392A6/haskel/first try/.cabal-sandbox/share/x86_64-linux-ghc-7.10.3/first-try-0.1.0.0"
libexecdir = "/media/meir/CC18F3AB18F392A6/haskel/first try/.cabal-sandbox/libexec"
sysconfdir = "/media/meir/CC18F3AB18F392A6/haskel/first try/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "first_try_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "first_try_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "first_try_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "first_try_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "first_try_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
