{-# LANGUAGE CPP #-}

import           Control.Monad                             (void, when)
import           Data.List                                 (isSuffixOf)
import           Distribution.PackageDescription           (HookedBuildInfo, PackageDescription (testSuites),
                                                            TestSuite (..))
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo        (ComponentName (..), LocalBuildInfo (..))
import           Distribution.Simple.Setup                 (BuildFlags (..), fromFlag)
import           Distribution.Simple.Utils
import           Distribution.Verbosity                    (Verbosity)
import           System.Directory                          (makeAbsolute)
import           System.Environment                        (getEnv, getEnvironment, setEnv)

#if MIN_VERSION_Cabal(2,0,0)

import           Distribution.Types.ComponentName          (ComponentName (CTestName))
import           Distribution.Types.ComponentRequestedSpec (ComponentRequestedSpec (OneComponentRequestedSpec))
import           Distribution.Types.PackageDescription     (enabledComponents)
import           Distribution.Types.UnqualComponentName    (mkUnqualComponentName)

#endif

buildingInCabal :: IO Bool
buildingInCabal = do
  parentProcess <- getEnv "_"
  return $ not ("stack" `isSuffixOf` parentProcess)

-- note: this only works in cabal, stack doesn't seem to pass these?

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)

willBuildLiveSuite :: PackageDescription -> Bool
willBuildLiveSuite = not . null . liveTestComponents
  where liveTestComponents = flip enabledComponents (OneComponentRequestedSpec (CTestName (mkUnqualComponentName "live")))

#else

willBuildLiveSuite :: PackageDescription  -> Bool
willBuildLiveSuite = any isLiveTest . testSuites
  where isLiveTest t = testName t == "live" && testEnabled t

#endif

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
         { buildHook = myBuildHook
         }

setupLiveTests :: Verbosity -> IO ()
setupLiveTests v = do
    convertAbi <- makeAbsolute "./test-support/convertAbi.sh"
    injectAddr <- makeAbsolute "./test-support/inject-contract-addresses.sh"
    putStrLn "Running truffle deploy and convertAbi before building tests"
    testCommand v "truffle" ["deploy"] Nothing
    testCommand v convertAbi [] Nothing
    testCommand v injectAddr [] (Just [("EXPORT_STORE", exportStore)])

testCommand :: Verbosity -> String -> [String] -> Maybe [(String, String)] -> IO ()
testCommand v prog args moreEnv = do
    env <- getEnvironment
    newWorkdir <- makeAbsolute "./test-support/"
    let allEnvs = case moreEnv of
                    Nothing   -> env
                    Just more -> more ++ env
    maybeExit $ rawSystemIOWithEnv v prog args (Just newWorkdir) (Just allEnvs) Nothing Nothing Nothing

exportStore :: String
exportStore = ".detected-contract-addresses"

myBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
myBuildHook pd lbi uh flags = do
    print $ buildArgs flags
    inCabal <- buildingInCabal
    let v = fromFlag $ buildVerbosity flags
        args = buildArgs flags
        isStackTest = not inCabal && "test:live" `elem` args
        isCabalTest = inCabal && willBuildLiveSuite pd && (null args || "live" `elem` args)
        hasLiveTestTarget = isStackTest || isCabalTest
    when hasLiveTestTarget $ setupLiveTests v
    buildHook simpleUserHooks pd lbi uh flags
