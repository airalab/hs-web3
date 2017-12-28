import           Control.Monad                      (void, when)
import           Data.List                          (isPrefixOf)
import           Data.Traversable                   (for)
import           Distribution.PackageDescription    (HookedBuildInfo, PackageDescription)
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo (LocalBuildInfo (..))
import           Distribution.Simple.Setup          (BuildFlags (..), TestFlags, fromFlag)
import           Distribution.Simple.Utils
import           Distribution.Verbosity             (Verbosity)
import           System.Environment                 (getEnvironment, setEnv)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
         { buildHook = myBuildHook
         }

rawCommand :: Verbosity -> String -> [String] -> Maybe [(String, String)] -> IO ()
rawCommand v prog args moreEnv = do
    env <- getEnvironment
    let allEnvs = case moreEnv of
                    Nothing   -> env
                    Just more -> more ++ env
    maybeExit $ rawSystemIOWithEnv v prog args Nothing (Just allEnvs) Nothing Nothing Nothing

exportStore :: String
exportStore = ".detected-contract-addresses"

myBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
myBuildHook pd lbi uh flags = when hasTestTarget $ do
    putStrLn "Running truffle deploy and convertAbi before building tests"
    rawCommand v "truffle" ["deploy"] Nothing
    rawCommand v "./convertAbi.sh" [] Nothing
    rawCommand v "./inject-contract-addresses.sh" [] (Just [("EXPORT_STORE", exportStore)])

    buildHook simpleUserHooks pd lbi uh flags

    where v = fromFlag $ buildVerbosity flags
          hasTestTarget = any ("test:" `isPrefixOf`) $ buildArgs flags
