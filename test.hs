#!/usr/bin/env runhaskell
{-# OPTIONS -Wall -Werror #-}

import Data.Functor
import Data.List
import System.Directory
import System.Exit
import System.IO
import System.Process

main :: IO ()
main = do
    let pythonModules = ["digital_library"]
    pythonFiles <- filter (".py" `isSuffixOf`) <$> getDirectoryContents "."
    -- ^ TODO recurse into subdirectories
    let python2Files = ["client.py"]
        python3Files = pythonFiles \\ python2Files

    let pep8Options = ["--show-source"]
    pep8 $ pep8Options ++ pythonFiles

    pyflakes2 python2Files
    pyflakes3 $ python3Files ++ pythonModules

    let pylintOptions = [ "--disable=locally-disabled"
                        , "--disable=missing-docstring"
                        , "--disable=no-init"
                        , "--disable=star-args"
                        , "--good-names=app,db"
                        , "--include-naming-hint=yes"
                        , "--output-format=colorized"
                        , "--reports=no"
                        ]
    pylint2 $ pylintOptions ++ python2Files
    pylint3 $ pylintOptions ++ python3Files ++ pythonModules

    pytest3 []

    putStrLn "OK"
  where
    pep8      = callProcess "pep8"
    pyflakes2 = callProcess "pyflakes"
    pyflakes3 = callProcess "pyflakes3"
    pylint2   = callProcess "pylint"
    pylint3   = callProcess "pylint3"
    pytest3   = callProcess "py.test-3"

    callProcess cmd args = do
        exitCode <- rawSystem cmd args
        case exitCode of
            ExitSuccess ->
                return ()
            ExitFailure code -> do
                hPutStrLn stderr $ cmd ++ " failed with code " ++ show code
                exitWith exitCode
