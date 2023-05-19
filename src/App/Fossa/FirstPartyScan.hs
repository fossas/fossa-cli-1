module App.Fossa.FirstPartyScan (
  runFirstPartyScan,
  firstPartyScanWithOrgInfo,
) where

import App.Fossa.Config.Analyze (StandardAnalyzeConfig (..), VendoredDependencyOptions (licenseScanPathFilters))
import App.Fossa.LicenseScanner (scanVendoredDep)
import App.Fossa.ManualDeps (VendoredDependency (..), findAndReadFossaDepsFile, ManualDependencies (vendoredDependencies))
import App.Types (FirstPartyScansFlag (..), FullFileUploads (FullFileUploads))
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Effect.FossaApiClient (FossaApiClient, getOrganization)
import Control.Effect.Lift (Lift)
import Control.Effect.StickyLogger (StickyLogger)
import Effect.Exec (Exec)
import Effect.ReadFS (Has, ReadFS, resolvePath')
import Fossa.API.Types (ApiOpts (..), Organization (..), blankOrganization)
import Path (Abs, Dir, Path, SomeBase (..), File)
import Srclib.Types (LicenseSourceUnit)
import Effect.Logger (Logger, logDebug, Pretty (pretty))
import Types (LicenseScanPathFilters (..), GlobFilter (GlobFilter))
import Data.Text (Text)
import Path.Extra
import Data.Maybe (catMaybes)
import Data.String.Conversion (ToString (toString))
import qualified Control.Carrier.Diagnostics as Diag
import Diag.Result

runFirstPartyScan ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has Exec sig m
  , Has ReadFS sig m
  ) =>
  Path Abs Dir ->
  Maybe ApiOpts ->
  StandardAnalyzeConfig ->
  m (Maybe LicenseSourceUnit)
runFirstPartyScan root maybeApiOpts cfg = do
  -- if we do not have api opts, then we act as if the org defaults to not running first-party scans
  -- but the FOSSA server supports first-party scans
  case maybeApiOpts of
    Nothing -> firstPartyScanMain root cfg defaultOrg
    Just apiOpts -> runFossaApiClient apiOpts $ firstPartyScanWithOrgInfo root cfg
  where
    defaultOrg = blankOrganization{orgDefaultsToFirstPartyScans = False, orgSupportsFirstPartyScans = True, orgRequiresFullFileUploads = False}

firstPartyScanWithOrgInfo ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has FossaApiClient sig m
  ) =>
  Path Abs Dir ->
  StandardAnalyzeConfig ->
  m (Maybe LicenseSourceUnit)
firstPartyScanWithOrgInfo root cfg = do
  org <- getOrganization
  firstPartyScanMain root cfg org

shouldRunFirstPartyScans :: (Has Diagnostics sig m) => StandardAnalyzeConfig -> Organization -> m Bool
shouldRunFirstPartyScans cfg org =
  case (firstPartyScansFlag cfg, orgDefaultsToFirstPartyScans org, orgSupportsFirstPartyScans org) of
    (FirstPartyScansOnFromFlag, _, False) -> fatalText "You provided the --experimental-force-first-party-scans flag but the FOSSA server does not support first-party scans"
    (_, _, False) -> pure False
    (FirstPartyScansOnFromFlag, _, True) -> pure True
    (FirstPartyScansOffFromFlag, _, True) -> pure False
    (FirstPartyScansUseDefault, orgDefault, True) -> pure orgDefault

firstPartyScanMain ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has Exec sig m
  , Has ReadFS sig m
  ) =>
  Path Abs Dir ->
  StandardAnalyzeConfig ->
  Organization ->
  m (Maybe LicenseSourceUnit)
firstPartyScanMain base cfg org = do
  runFirstPartyScans <- shouldRunFirstPartyScans cfg org
  manualDeps <- findAndReadFossaDepsFile base
  let vdep = VendoredDependency "first-party" "." Nothing
      fullFileUploads = FullFileUploads $ orgRequiresFullFileUploads org
  pathFilters <- mergePathFilters base manualDeps (licenseScanPathFilters $ vendoredDeps cfg)
  case runFirstPartyScans of
    (True) -> do
      _ <- logDebug "Running a first-party license scan on the code in this repository. Licenses found in this repository will show up as 'Directly in code' in the FOSSA UI"
      _ <- logDebug . pretty $ "path filters = " ++ show pathFilters
      Just <$> scanVendoredDep base pathFilters fullFileUploads vdep
    (False) -> pure Nothing

-- mergePathFilters takes the existing filters from the config and merges them with filters constructed by looking at the vendored dependencies
-- We do this because we want to skip scanning the contents of all directories that are vendored dependencies,
-- and we also want to add all vendored dependencies that point at files to `licenseScanCompressedFileExclude`,
-- so that we do not decompress and license-scan them
mergePathFilters ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m) =>
  Path Abs Dir -> Maybe ManualDependencies -> Maybe LicenseScanPathFilters -> m (Maybe LicenseScanPathFilters)
mergePathFilters base maybeManualDeps existingPathFilters = do
  case (maybeManualDeps, existingPathFilters) of
    (Nothing, Nothing) -> pure Nothing
    (Just manualDeps, Nothing) -> do
      fromManual <- filtersFromManualDeps base manualDeps
      pure $ Just fromManual
    (Nothing, Just existingFilters) -> pure $ Just existingFilters
    (Just manualDeps, Just existingFilters) -> do
      fromManual <- filtersFromManualDeps base manualDeps
      let merged = mergeFilters fromManual existingFilters
      pure $ Just merged
  where
    -- mergeFilters takes the filters from manualDeps and the existing filters from the config and merges them
    -- the existing filters are the only one of the two that can contain licenseScanPathFiltersOnly entries,
    -- but both can contain licenseScanPathFiltersExclude or licenseScanCompressedFilesExclude entries
    mergeFilters :: LicenseScanPathFilters -> LicenseScanPathFilters -> LicenseScanPathFilters
    mergeFilters manualDepsFilters existingFilters =
      existingFilters
      { licenseScanPathFiltersExclude = (licenseScanPathFiltersExclude manualDepsFilters) <> (licenseScanPathFiltersExclude existingFilters)
      , licenseScanCompressedFilesExclude = (licenseScanCompressedFilesExclude manualDepsFilters) <> (licenseScanCompressedFilesExclude existingFilters)
      }

-- create LicenseScanPathFilters by looking at the vendored dependencies
-- We want to skip scanning all directories that are vendored dependencies,
-- and we also want to add all vendored dependencies that point at files to `licenseScanCompressedFileExclude`,
-- so that we do not decompress and license-scan them
filtersFromManualDeps ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m) =>
  Path Abs Dir -> ManualDependencies -> m LicenseScanPathFilters
filtersFromManualDeps base manualDeps = do
  let paths = map vendoredPath $ vendoredDependencies manualDeps
  vendoredDepFiles <- traverse (fullPathToVendoredDepFile base) paths
  let excludes = concatMap (\path -> [GlobFilter (path <> "/*"), GlobFilter (path <> "/**")]) paths
  pure LicenseScanPathFilters
    { licenseScanPathFiltersOnly = []
    , licenseScanPathFiltersExclude = excludes
    , licenseScanCompressedFilesExclude = catMaybes vendoredDepFiles
    }

fullPathToVendoredDepFile ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m) =>
  Path Abs Dir -> Text -> m (Maybe (Path Abs File))
fullPathToVendoredDepFile root p = do
  scanPath <- Diag.runDiagnostics $ resolvePath' root $ toString p
  case scanPath of
    Success _ (SomeFile (Abs path)) -> pure $ Just path
    Success _ (SomeFile _) -> pure Nothing
    Success _ (SomeDir _) -> pure Nothing
    _ -> pure Nothing

