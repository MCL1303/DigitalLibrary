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

    let pep8Options = ["--show-source"]
    pep8 $ pep8Options ++ ["."]

    pyflakes $ pythonFiles ++ pythonPackages

    let pylintOptions = [ "--disable=bad-continuation"
                        , "--disable=locally-disabled"
                        , "--disable=missing-docstring"
                        , "--disable=star-args"
                        , "--dummy-variables-rgx=_.*"
                        , "--extension-pkg-whitelist=PyQt4,PyQt5"
                        , "--good-names=app,closeEvent,db"
                        , "--include-naming-hint=yes"
                        , "--output-format=colorized"
                        , "--reports=no"
                        , "--variable-rgx=[a-z_][a-z0-9_]{,30}$"
                        ]
    pylint $ pylintOptions ++ pythonFiles ++ pythonPackages

    pytest []

    putStrLn "OK"

  where
    pep8      = callProcess "pep8"
    pyflakes  = callProcess "pyflakes3"
    pylint    = callProcess "pylint3"
    pytest    = callProcess "py.test-3"

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
