#!/usr/bin/env runhaskell
{-# OPTIONS -Wall -Werror #-}

import Data.List
import System.Directory
import System.Exit
import System.IO
import System.Process   ( rawSystem )

main :: IO ()
main = do
    pythonFiles <- filter (".py" `isSuffixOf`) `fmap` getDirectoryContents "."
    let pythonPackages = ["digital_library"]
        python2Files = ["client.py"]
        python3Files = pythonFiles \\ python2Files

    let pep8Options = ["--show-source"]
    pep8 $ pep8Options ++ ["."]

    pyflakes2 python2Files
    pyflakes3 $ python3Files ++ pythonPackages

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
    pylint3 $ pylintOptions ++ python3Files ++ pythonPackages

    pytest3 []

    putStrLn "OK"

  where
    pep8      = callProcess "pep8"
    pyflakes2 = callProcess "pyflakes"
    pyflakes3 = callProcess "pyflakes3"
    pylint2   = callProcess "pylint"
    pylint3   = callProcess "pylint3"
    pytest3   = callProcess "py.test-3"

    -- |  backport of callProcess from process-1.2
    --    for compatibility with Ubuntu 15.04/process-1.1.0.2
    callProcess cmd args = do
        exitCode <- rawSystem cmd args
        case exitCode of
            ExitSuccess ->
                return ()
            ExitFailure code -> do
                hPutStrLn stderr $ cmd ++ " failed with code " ++ show code
                exitWith exitCode
