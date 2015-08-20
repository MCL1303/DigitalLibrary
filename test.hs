#!/usr/bin/env runhaskell
{-# OPTIONS -Wall -Werror #-}

import Data.Foldable
import Data.Functor
import Data.List
import System.Directory
import System.Exit
import System.IO
import System.Process

main :: IO ()
main = do
    pythonFiles <- filter (".py" `isSuffixOf`) <$> getDirectoryContents "."
    (python2Files, python3Files) <- partitionM isPython2File pythonFiles

    let pep8Options = ["--show-source"]
    pep8 $ pep8Options ++ pythonFiles

    let pylintOptions = [ "--disable=missing-docstring"
                        , "--disable=star-args"
                        , "--disable=too-many-public-methods"
                        , "--disable=too-few-public-methods"
                        , "--disable=no-init"
                        , "--good-names=app,db"
                        , "--include-naming-hint=yes"
                        , "--output-format=colorized"
                        , "--reports=no"
                        ]
    pylint2 $ pylintOptions ++ python2Files
    pylint3 $ pylintOptions ++ python3Files

    pyflakes2 python2Files
    pyflakes3 python3Files

    pytest3 []

    putStrLn "OK"
  where
    pep8      = callProcess "pep8"
    pyflakes2 = callProcess "pyflakes"
    pyflakes3 = callProcess "pyflakes3"
    pylint2   = callProcess "pylint"
    pylint3   = callProcess "pylint3"
    pytest3   = callProcess "py.test-3"

    isPython2File file =
        withFile file ReadMode $ \h ->
            ("python2" `isInfixOf`) <$> hGetLine h

    callProcess cmd args = do
        exitCode <- rawSystem cmd args
        case exitCode of
            ExitSuccess ->
                return ()
            ExitFailure code -> do
                hPutStrLn stderr $ cmd ++ " failed with code " ++ show code
                exitWith exitCode

    partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
    partitionM p = foldrM select ([], [])
      where
        select x ~(ts, fs) = do
            px <- p x
            return $ if px  then (x:ts, fs)
                            else (ts, x:fs)
