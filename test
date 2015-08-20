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

    pep8 pythonFiles
    pylint2 python2Files
    pylint3 python3Files
    pytest3

    putStrLn "OK"
  where
    pep8 files = callProcess "pep8" $ options ++ files
      where options = ["--show-source"]

    pylint2 files = callProcess "pylint" $ pylintOptions ++ files
    pylint3 files = callProcess "pylint3" $ pylintOptions ++ files
    pylintOptions = [ "--disable=invalid-name"  -- TODO don't ignore
                    , "--disable=line-too-long"  -- TODO don't ignore
                    , "--disable=missing-docstring"
                    , "--disable=star-args"
                    , "--include-naming-hint=yes"
                    , "--output-format=colorized"
                    , "--reports=no"
                    ]

    pytest3 = callProcess "py.test-3" []

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
