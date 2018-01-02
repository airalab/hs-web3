import           Control.Monad                      (void, when)
import           Data.List                          (isSuffixOf)
import           Distribution.PackageDescription    (HookedBuildInfo, PackageDescription (testSuites), TestSuite (..))
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo (ComponentName (..), LocalBuildInfo (..))
import           Distribution.Simple.Setup          (BuildFlags (..), fromFlag)
import           Distribution.Simple.Utils
import           Distribution.Verbosity             (Verbosity)
import           System.Directory                   (makeAbsolute)
import           System.Environment                 (getEnv, getEnvironment, setEnv)

buildingInCabal :: IO Bool
buildingInCabal = do
  parentProcess <- getEnv "_"
  return $ not ("stack" `isSuffixOf` parentProcess)

-- note: this only works in cabal, stack doesn't seem to pass these?
willBuildLiveSuite :: PackageDescription  -> Bool
willBuildLiveSuite = any isLiveTest . testSuites
  where isLiveTest t = testName t == "live" && testEnabled t

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
    inCabal <- buildingInCabal
    let v = fromFlag $ buildVerbosity flags
        args = buildArgs flags
        isStackTest = not inCabal && "test:live" `elem` args
        isCabalTest = inCabal && willBuildLiveSuite pd && (null args || "live" `elem` args)
        hasLiveTestTarget = isStackTest || isCabalTest
    when hasLiveTestTarget $ setupLiveTests v
    buildHook simpleUserHooks pd lbi uh flags
