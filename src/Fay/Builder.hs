module Fay.Builder
 ( readPackageDescription
 , build
 , listField
 , listField_
 , field
 , field_
 , fayConfig
 , defaultFayHook
 , postBuildHook
 ) where

import Control.Monad
import Data.Default
import Data.List
import Data.List.Split
import Data.Maybe
import System.Directory
import System.FilePath
import qualified Data.Text as T

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Fay
import Fay.Compiler.Config
import qualified Distribution.PackageDescription.Parse as PD (readPackageDescription)
import qualified Distribution.Verbosity                as Verbosity

-- | Default parsing of a Cabal file.
readPackageDescription :: FilePath -> IO PackageDescription
readPackageDescription = PD.readPackageDescription Verbosity.silent >=> return . packageDescription

-- | Compile code
build :: PackageDescription -> Maybe FilePath -> IO ()
build packageDesc pkgDb = do
  let packages     = listField_ "x-fay-packages"      packageDesc
      roots        = listField_ "x-fay-root-modules"  packageDesc
      includePaths = listField_ "x-fay-include-paths" packageDesc
      sourceDir    = field_     "x-fay-source-dir"    packageDesc
      outputDir    = field_     "x-fay-output-dir"    packageDesc

  forM_ (zip roots [(1::Int)..]) $ \(name, i) -> do
    let candidate = sourceDir </> name <.> "hs"
        out       = outputDir </> name <.> "js"
    exists <- doesFileExist candidate
    if exists
      then do
        putStrLn $ "fay: [" ++ show i ++ " of " ++ show (length roots) ++ "] Compiling " ++ name ++ " ( " ++ candidate ++ ", " ++ out ++ " )"
        compileFromTo (fayConfig pkgDb packages sourceDir includePaths) candidate (Just out)
      else
        error $ "fay-builder: Could not find " ++ candidate

-- | Try to read a comma separated field
listField :: String -> PackageDescription -> Maybe [String]
listField key = fmap (map strip . splitOn ",") . field key

-- | Read the value of a comma separated field, gives an empty list if the field is not present.
listField_ :: String -> PackageDescription -> [String]
listField_ fn = fromMaybe [] . listField fn

-- | Tyr to read a field's value
field :: String -> PackageDescription -> Maybe String
field key = fmap strip . lookup key . customFieldsPD

-- | Force reading of a field, fails if it doesn't exist
field_ :: String -> PackageDescription -> String
field_ key = fromMaybe (error $ key ++ "is  missing") . field key

-- | Default config, TODO make this optional
fayConfig :: Maybe FilePath -> [String] -> FilePath -> [FilePath] -> CompileConfig
fayConfig pkgDb packages dir includePs =
    addConfigDirectoryIncludePaths (dir : includePs)
  . addConfigPackages              packages
  $ def { Fay.configWall        = True
        , Fay.configPrettyPrint = True
        , Fay.configPackageConf = pkgDb
        }

-- | Default build hook for your Setup.hs
defaultFayHook :: IO ()
defaultFayHook = defaultMainWithHooks simpleUserHooks { postBuild = postBuildHook }

-- | Default post build hook for your Setup.hs
postBuildHook :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postBuildHook _ _ packageDesc localBuildInfo = do
  putStrLn "Building Fay client ..."
  build packageDesc  (findSpecificPackageDb localBuildInfo)
  putStrLn "Finished building Fay client"
  where
    findSpecificPackageDb =
        fmap (\(SpecificPackageDB p) -> p)
      . find (\db -> case db of
          SpecificPackageDB{} -> True
          _                   -> False)
      . withPackageDB

-- | Strip leading and trailing whitespace
strip :: String -> String
strip = T.unpack . T.strip . T.pack
