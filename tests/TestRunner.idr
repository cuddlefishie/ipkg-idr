-- SPDX-FileCopyrightText: 2021 The ipkg-idr developers
--
-- SPDX-License-Identifier: MPL-2.0

module TestRunner

import Language.IPKG

import System
import System.File

parseIPKGFile : (path : String) -> IO ()
parseIPKGFile path = do
    putStrLn $ "Parsing file " ++ show path

    Right src <- readFile path
        | Left err => do
            putStrLn $ "Error reading file: " ++ show err
            pure ()

    Right pkg <- pure $ parseIPKG src
        | Left err => do
            putStrLn $ "Error parsing file: " ++ err
            pure ()
    
    printLn pkg

main : IO ()
main = do
    (_ :: args) <- getArgs
        | [] => pure ()
    for_ args \path => do
        parseIPKGFile path