{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( debugArgs
    , isMarkDownStr
    , isMarkDownFile
    , isDebug
    , strToLower
    , validateWithTests
    , printIfDoesntExist
    , validateFileExists
    ) where

import           Control.Applicative (liftA2)
import qualified Data.Char           as C
import           System.FilePath

-- for system environment, etc.
import           System.Directory    (doesDirectoryExist, doesFileExist)
import           System.Environment  (getArgs, getEnvironment)

-- Monad helpers
import           Control.Monad       (foldM, liftM2)

isMarkDownStr :: String -> Bool
isMarkDownStr = liftA2 (||) (== "md") (== "markdown") . strToLower

isMarkDownExt :: String -> Bool
isMarkDownExt = isMarkDownStr . safeTail

safeTail :: [a] -> [a]
safeTail [] = []
safeTail xs = tail xs

isMarkDownFile :: FilePath -> Bool
isMarkDownFile = isMarkDownExt . takeExtension


debugArgs :: IO ()
debugArgs = do
    args <- getArgs
    putStrLn "The args passed to the app on the command line were:"
    putStrLn $ unwords args
    putStrLn "------"


isDebug :: IO Bool
isDebug = do
    envVars <- getEnvironment
    let debug = filter ((=="DEBUG").fst) envVars
    pure $ not (null debug) && case head debug of
        (_,"") -> False
        _      -> True


strToLower :: String -> String
strToLower = map C.toLower


validateWithTests :: a -> [a -> IO Bool] -> IO Bool
validateWithTests a = foldM ander True
  where
      mAnd = liftM2 (&&)
      ander acc test = pure acc `mAnd` test a


validateFileExists :: (a -> String)
                   -> String
                   -> a
                   -> IO Bool
validateFileExists = validateThingExists doesFileExist


validateDirExists :: (a -> FilePath)
                  -> String
                  -> a
                  -> IO Bool
validateDirExists = validateThingExists doesDirectoryExist


validateThingExists :: (FilePath -> IO Bool)
                    -> (a -> FilePath)
                    -> String
                    -> a
                    -> IO Bool
validateThingExists test f errorStr args = do
    let path = f args
    exists <- test path
    printIfDoesntExist exists path errorStr
    pure exists


printIfDoesntExist :: Bool -> String -> String -> IO ()
printIfDoesntExist True _ _   = pure ()
printIfDoesntExist False path prefix = putStrLn $ prefix ++ path ++ " doesn't exist!"
