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
                        , "--extension-pkg-whitelist=PyQt5"
                        , "--good-names=app,db"
                        , "--include-naming-hint=yes"
                        , "--output-format=colorized"
                        , "--reports=no"
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
