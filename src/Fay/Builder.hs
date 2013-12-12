module Fay.Builder where

import Control.Monad
import Data.Default
import Data.List.Split
import Data.Maybe
import Distribution.PackageDescription
import Fay
import Fay.Compiler.Config
import System.Directory
import System.FilePath
import qualified Data.Text              as T
import qualified Distribution.Verbosity as Verbosity
import qualified Distribution.PackageDescription as PD
import qualified Distribution.PackageDescription.Parse as PD

readPackageDescription :: FilePath -> IO PackageDescription
readPackageDescription = PD.readPackageDescription Verbosity.silent >=> return . PD.packageDescription

foo :: PackageDescription -> Maybe FilePath -> IO ()
foo packageDesc pkgDb = do
  let packages     = listField "x-fay-packages"      packageDesc
      roots        = listField "x-fay-root-modules"  packageDesc
      includePaths = listField "x-fay-include-paths" packageDesc
      mSourceDir   = field     "x-fay-source-dir"    packageDesc
      mOutputDir   = field     "x-fay-output-dir"    packageDesc
      sourceDir    = fromMaybe (error "x-fay-source-dir missing") mSourceDir
      outputDir    = fromMaybe (error "x-fay-output-dir missing") mOutputDir

  forM_ (zip roots [(1::Int)..]) $ \(name, i) -> do
    let candidate = sourceDir </> name <.> "hs"
        out       = outputDir </> name <.> "js"
    exists <- doesFileExist candidate
    if exists
      then do
        putStrLn $ "fay: [" ++ show i ++ " of " ++ show (length roots) ++ "] Compiling " ++ name ++ " ( " ++ candidate ++ ", " ++ out ++ " )"
        compileFromTo (config pkgDb packages sourceDir includePaths) candidate (Just out)
      else
        error $ "fay-builder: Could not find " ++ candidate

listField :: String -> PackageDescription -> [String]
listField key = fromMaybe [] . fmap (map strip . splitOn ",") . field key

field :: String -> PackageDescription -> Maybe String
field key = fmap strip . lookup key . customFieldsPD

config :: Maybe FilePath -> [String] -> FilePath -> [FilePath] -> CompileConfig
config pkgDb packages dir includePs =
    addConfigDirectoryIncludePaths (dir : includePs)
  . addConfigPackages              packages
  $ def { Fay.configWall        = True
        , Fay.configPrettyPrint = True
        , Fay.configPackageConf = pkgDb
        }

strip :: String -> String
strip = T.unpack . T.strip . T.pack
