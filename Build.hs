{-# LANGUAGE ScopedTypeVariables #-}

module Build(main) where

import System.Directory
import System.FilePath
import System.Exit
import Control.Monad.Extra
import Development.Rattle
import UnliftIO.Environment
import Data.Char
import Data.List.Extra
import qualified Debug.Trace as T

import Unix
import Configure
import Config
import Mach

quickOptions j = RattleOptions ".rattlequick" (Just "quick") "m1" False j [] [("PWD", ".")] Nothing False

configOptions m j = RattleOptions (".rattleconfig" ++ m) (Just "config") "m1" False j [] [("PWD", ".")] Nothing False

buildOptions j = RattleOptions ".rattlebuild" (Just "build") "m1" False j [] [("PWD", ".")] Nothing False

installOptions j = RattleOptions ".rattleinstall" (Just "install") "m1" False j [] [("PWD", ".")] Nothing False

machsBootquick = let (*) = (,) in
  [(Mach True A6 LE) * Unix.bootquick]

machsBuild = let (*) = (,) in
  [(Mach True A6 LE) * Unix.build]

machsInstall = let (*) = (,) in
  [(Mach True A6 LE) * Unix.install]

main :: IO ()
main = do
  args <- getArgs
  config <- case args of
              "--configure":rest -> do
                let (j, xs) = case rest of
                                "--j=":j:x -> (read j, x)
                                x -> (0, x)
                let m = case xs of
                          "--pb":_ -> "pb"
                          _ -> ""
                
                config <- rattleRun (configOptions m j) $ withArgs xs configure
                ifM (doesFileExist ".config")
                  (do
                      removeFile ".config"
                      writeFile ".config" $ show config)
                  (writeFile ".config" $ show config)
                exitSuccess
                  
              _ -> ifM (doesFileExist ".config")
                   (do
                       str <- readFile ".config"
                       return $ read str) -- read config from file
                   (do
                       putStrLn $ "Could not find configuration; please run with --configure"
                       exitFailure)
  case args of
    [] -> exitSuccess
    "--bootquick":m2:rest -> do
      let j = case rest of
                "--j=":j:_ -> read j
                _ -> 0
      mach <- readMach (trim m2)
      case lookup mach machsBootquick of
        Just act -> do
          withCurrentDirectory (showMach $ m config) $ rattleRun (quickOptions j) $ act mach config j
        Nothing -> do
          putStrLn $ m2 ++ " is currently an unsupported machine type.  Supported machine types are: " ++ unwords (map (show . fst) machsBuild)
          exitFailure
    "--build":rest -> case lookup (m config) machsBuild of
                        Just act -> do
                          let j = case rest of
                                "--j=":j:_ -> read j
                                _ -> 0
                          withCurrentDirectory (showMach $ m config) $ rattleRun (buildOptions j) $ act config j
                        Nothing -> do
                          putStrLn $ "Unknown machine type, expected one of\n " ++ unwords (map (show . fst) machsBuild)
                          exitFailure

    "--install":v:rest -> case lookup (m config) machsInstall of
                            Just act -> do
                              let j = case rest of
                                        "--j=":j:_ -> read j
                                        _ -> 0
                              let dir = "boot-final" </> (showMach $ m config) in
                                withCurrentDirectory (showMach $ m config) $ rattleRun (installOptions j) $ act v dir config
                            Nothing -> do
                              putStrLn $ "Unknown machine type, expected one of\n " ++ unwords (map (show . fst) machsInstall)
                              exitFailure
    _ -> do
      putStrLn $ "Unrecognized option: " ++ show args
      exitFailure
