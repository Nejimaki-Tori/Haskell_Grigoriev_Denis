module Main (main) where

import Read(readDFA, setTr)

import Print (printDFA, printGrammar)

import TransformToGrammar(makeGrammar)

import NfaToDfa (buildDFA)

import Check(checkAll, checkStr)

import Control.Exception

import System.IO

import System.Exit (exitSuccess)

import Control.Monad (when)

-- main
main :: IO ()
main = do
        putStrLn $ "Enter the name of the input file:"
        name <- getLine
        handleRead <- openFile name ReadMode
        autoTxt <- try (hGetContents handleRead) :: IO(Either SomeException String)
        case autoTxt of
                Left ex -> putStrLn $ "Error, cant open the file: " ++ show ex
                Right contents -> do
                        let checkString = checkStr contents
                        when (checkString) $ do
                                putStrLn "Wrong transition in here!"
                                exitSuccess
                        let nfa = setTr (readDFA contents) contents
                        let check = checkAll nfa
                        when (check /= "") $ do
                                putStrLn check
                                exitSuccess
                        let dfa = buildDFA nfa
                        handleWrite <- openFile ("output/" ++ "grammar.txt") WriteMode
                        result <- try (hPutStrLn handleWrite (printGrammar (makeGrammar dfa))) :: IO(Either SomeException ())
                        hClose handleWrite
                        case result of
                                Left ex -> putStrLn $ "Error, cant open the file: " ++ show ex
                                Right _ -> do
                                        handleAuto <- openFile ("output/" ++ "DFA.txt") WriteMode
                                        auto <- try (hPutStrLn handleAuto (printDFA dfa)) :: IO(Either SomeException ())
                                        hClose handleAuto
                                        case auto of
                                                Left ex -> putStrLn $ "Error, cant open the file: " ++ show ex
                                                Right _ -> putStrLn $ "OK!"
        return ()
