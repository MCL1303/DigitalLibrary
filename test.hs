#!/usr/bin/env runhaskell
{-# OPTIONS -Wall -Werror #-}

{-  Digital Library — a digital book management system
    Copyright (C) 2015  Yuriy Syrovetskiy <cblp@cblp.su>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>. -}


import Data.List
import System.Directory
import System.Exit
import System.IO
import System.Process   ( rawSystem )

main :: IO ()
main = do
    pythonFiles <- filter (".py" `isSuffixOf`) `fmap` getDirectoryContents "."
    let pythonPackages = ["digital_library"]

    let pep8Options = ["--show-source", "--ignore=E251"]
    pep8 $ pep8Options ++ ["."]

    pyflakes $ pythonFiles ++ pythonPackages

    let pylintOptions = [ "--output-format=colorized"
                        , "--rcfile=.pylintrc"
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
