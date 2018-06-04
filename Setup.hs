import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Package (packageVersion, packageName, PackageIdentifier(..), PackageName, unPackageName)
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks, UserHooks(..))
import Distribution.Simple.BuildPaths (autogenComponentModulesDir)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, withExeLBI)
import Distribution.Simple.Setup (BuildFlags(..), fromFlag)
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose, rewriteFile)
import System.FilePath ((</>), (<.>))
import Distribution.Version (showVersion)

main = defaultMainWithHooks packageInfoUserHooks

packageInfoUserHooks :: UserHooks
packageInfoUserHooks =
    simpleUserHooks {
        buildHook = genPackageInfoHook
    }

app_name :: PackageIdentifier -> String
app_name packageInfo = (unPackageName $ packageName packageInfo)

genPackageInfoHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
genPackageInfoHook pkg lbi uhs bfs= do
    withExeLBI pkg lbi $ \_ clbi -> do
        let compModDir = autogenComponentModulesDir lbi clbi
        createDirectoryIfMissingVerbose (fromFlag $ buildVerbosity bfs) True compModDir
        let packageInfoModulePath = compModDir </> cfg_name <.> "hs"
        rewriteFile packageInfoModulePath generate
    buildHook simpleUserHooks pkg lbi uhs bfs
    where cfg_name = "PackageInfo"
          generate = "module " ++ cfg_name ++ " where\n" ++
                     "\n" ++
                     "version     = " ++ (show $ showVersion $ packageVersion $ package pkg) ++ "\n" ++
                     "appName     = " ++ (show $ app_name $ package pkg) ++ "\n" ++
                     "synopsis    = " ++ (show $ synopsis pkg) ++ "\n"
