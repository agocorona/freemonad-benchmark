module Paths_freemonad_benchmark (
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
version = Version [1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\magocoal\\OneDrive\\Haskell\\stuff\\freemonad-benchmark\\.stack-work\\install\\x86_64-windows\\lts-3.7\\7.10.2\\bin"
libdir     = "C:\\Users\\magocoal\\OneDrive\\Haskell\\stuff\\freemonad-benchmark\\.stack-work\\install\\x86_64-windows\\lts-3.7\\7.10.2\\lib\\x86_64-windows-ghc-7.10.2\\freemonad-benchmark-1-7JK7Hrfk5VEJu29YyFbtnK"
datadir    = "C:\\Users\\magocoal\\OneDrive\\Haskell\\stuff\\freemonad-benchmark\\.stack-work\\install\\x86_64-windows\\lts-3.7\\7.10.2\\share\\x86_64-windows-ghc-7.10.2\\freemonad-benchmark-1"
libexecdir = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\freemonad-benchmark-1-7JK7Hrfk5VEJu29YyFbtnK"
sysconfdir = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "freemonad_benchmark_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "freemonad_benchmark_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "freemonad_benchmark_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "freemonad_benchmark_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "freemonad_benchmark_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
