import Data.Maybe

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription

import System.Directory(getCurrentDirectory)
import System.Process
import Debug.Trace

main = defaultMainWithHooks simpleUserHooks { confHook = myConfHook }

myConfHook (pkg0, pbi) flags = do
 
 installDir <- buildBoehm

 lbi <- confHook simpleUserHooks (pkg0, pbi) flags
 let lpd        = localPkgDescr lbi
 let lib        = fromJust (library lpd)
 let libbi      = libBuildInfo lib
 let custom_bi  = customFieldsBI libbi
 let libbi' = libbi
      {  extraLibDirs = (installDir ++ "/lib") : extraLibDirs libbi,
         includeDirs  = (installDir ++ "/include") : includeDirs libbi
      }
 let lib' = lib { libBuildInfo = libbi' }
 let lpd' = lpd { library = Just lib' }
 return $ lbi { localPkgDescr = lpd' }

buildBoehm :: IO String
buildBoehm = do
  wd <- getCurrentDirectory
  let installDir = wd ++ "/gc"
  print ("Installing Boehm to: " ++ installDir)
  _ <- runCommand ("./makeboehm.sh " ++ installDir) >>= waitForProcess
  return installDir
