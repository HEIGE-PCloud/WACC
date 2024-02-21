{-# LANGUAGE DeriveDataTypeable #-}

-- https://bitbucket.org/jstolarek/tasty-program/src/master/src/Test/Tasty/Program.hs
-- Copyright (c) 2014, Jan Stolarek

-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:

--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.

--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.

--     * Neither the name of Jan Stolarek nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

{- | This module provides a function that tests whether a program can
be run successfully. For example if you have 'foo.hs' source file:

> module Foo where
>
> foo :: Int
> foo = 5

you can test whether GHC can compile it:

> module Main (
>   main
>  ) where
>
> import Test.Tasty
> import Test.Tasty.Program
>
> main :: IO ()
> main = defaultMain $ testGroup "Compilation with GHC" $ [
>     testProgram "Foo" "ghc" ["-fforce-recomp", "foo.hs"] Nothing
>   ]

Program's output and error streams are ignored.
-}
module Test.Lib.Program
  ( testProgram
  , CatchStderr (..)
  , CatchStdout (..)
  )
where

import Control.DeepSeq (deepseq)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.IO (hGetContents)
import System.Process (runInteractiveProcess, waitForProcess)
import Test.Tasty.Options
  ( IsOption (..)
  , OptionDescription (..)
  , flagCLParser
  , lookupOption
  , safeRead
  )
import Test.Tasty.Providers
  ( IsTest (..)
  , Result
  , TestName
  , TestTree
  , singleTest
  , testFailed
  , testPassed
  )

data TestProgram = TestProgram String [String] (Maybe FilePath) ExitCode
  deriving (Typeable)

{- | Create test that runs a program with given options. Test succeeds
if program terminates successfully.
-}
testProgram
  :: TestName
  -- ^ Test name
  -> String
  -- ^ Program name
  -> [String]
  -- ^ Program options
  -> Maybe FilePath
  -- ^ Optional working directory
  -> ExitCode
  -- ^ Expected exit code
  -> TestTree
testProgram testName program opts workingDir exitCode =
  singleTest testName (TestProgram program opts workingDir exitCode)

instance IsTest TestProgram where
  run opts (TestProgram program args workingDir exitCode) _ = do
    execFound <- findExecutable program

    let
      CatchStderr catchStderr = lookupOption opts
    let
      CatchStdout catchStdout = lookupOption opts

    case execFound of
      Nothing -> return $ execNotFoundFailure program
      Just progPath -> runProgram progPath args workingDir catchStderr catchStdout exitCode

  testOptions =
    return
      [Option (Proxy :: Proxy CatchStderr), Option (Proxy :: Proxy CatchStdout)]

newtype CatchStderr = CatchStderr Bool deriving (Show, Typeable)

instance IsOption CatchStderr where
  defaultValue = CatchStderr False
  parseValue = fmap CatchStderr . safeRead
  optionName = return "catch-stderr"
  optionHelp = return "Catch standart error of programs"
  optionCLParser = flagCLParser (Just 'e') (CatchStderr True)

newtype CatchStdout = CatchStdout Bool deriving (Show, Typeable)

instance IsOption CatchStdout where
  defaultValue = CatchStdout False
  parseValue = fmap CatchStdout . safeRead
  optionName = return "catch-stdout"
  optionHelp = return "Catch standart outor of programs"
  optionCLParser = flagCLParser (Just 'o') (CatchStdout True)

rawExitCode :: ExitCode -> Int
rawExitCode ExitSuccess = 0
rawExitCode (ExitFailure code) = code

{- | Run a program with given options and optional working directory.
Return success if program exits with success code.
-}
runProgram
  :: String
  -- ^ Program name
  -> [String]
  -- ^ Program options
  -> Maybe FilePath
  -- ^ Optional working directory
  -> Bool
  -- ^ Whether to print stderr on error
  -> Bool
  -- ^ Whether to print stdout on error
  -> ExitCode
  -- ^ Expected exit code
  -> IO Result
runProgram program args workingDir catchStderr catchStdout exitCode = do
  (_, stdoutH, stderrH, pid) <-
    runInteractiveProcess program args workingDir Nothing

  stderr <-
    if catchStderr then fmap Just (hGetContents stderrH) else return Nothing
  stdout <-
    if catchStdout then fmap Just (hGetContents stdoutH) else return Nothing
  ecode <- stderr `deepseq` waitForProcess pid
  if ecode == exitCode
    then return success
    else return $ exitFailure program args (rawExitCode ecode) stderr stdout

-- | Indicates successful test
success :: Result
success = testPassed ""

-- | Indicates that program does not exist in the path
execNotFoundFailure :: String -> Result
execNotFoundFailure file =
  testFailed $ "Cannot locate program " ++ file ++ " in the PATH"

-- | Indicates that program failed with an error code
exitFailure
  :: String -> [String] -> Int -> Maybe String -> Maybe String -> Result
exitFailure file args code stderr stdout =
  testFailed $
    "Program "
      ++ unwords (file : args)
      ++ " failed with code "
      ++ show code
      ++ case stderr of
        Nothing -> ""
        Just s -> "\n Stderr was: \n" ++ s
      ++ case stdout of
        Nothing -> ""
        Just s -> "\n Stdout was: \n" ++ s
