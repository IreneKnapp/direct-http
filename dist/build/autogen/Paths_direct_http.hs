module Paths_direct_http (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [1,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/cabal/bin"
libdir     = "/usr/local/cabal/lib/direct-http-1.0/ghc-6.12.1.20100203"
datadir    = "/usr/local/cabal/share/direct-http-1.0"
libexecdir = "/usr/local/cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "direct_http_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "direct_http_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "direct_http_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "direct_http_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
