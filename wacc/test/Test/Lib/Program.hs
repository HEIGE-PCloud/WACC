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
  , ignoreOutput
  , TestProgram (..)
  , runTestProgram
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.DeepSeq (deepseq)
import Data.Foldable (for_)
import Data.Typeable (Typeable)
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.IO (hGetContents, hPutStr)
import System.Process (runInteractiveProcess, terminateProcess, waitForProcess)
import Test.Tasty.Providers
  ( IsTest (..)
  , Result
  , TestName
  , TestTree
  , singleTest
  , testFailed
  , testPassed
  )

data TestProgram
  = TestProgram
      String
      [String]
      (Maybe FilePath)
      (Maybe String)
      ExitCode
      CheckOutput
      CheckOutput
      Int
  deriving (Typeable)

type CheckOutput = String -> (Bool, String)

ignoreOutput :: String -> (Bool, String)
ignoreOutput _ = (True, "")

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
  -> Maybe String
  -- ^ Input
  -> ExitCode
  -- ^ Expected exit code
  -> CheckOutput
  -- ^ A function to check whether the stderr is correct
  -> CheckOutput
  -- ^ A function to check whether the stdout is correct
  -> Int
  -- ^ Timeout in microseconds
  -> TestTree
testProgram testName program opts workingDir input exitCode checkStderr checkStdout timeout =
  singleTest
    testName
    ( TestProgram
        program
        opts
        workingDir
        input
        exitCode
        checkStderr
        checkStdout
        timeout
    )

runTestProgram :: TestProgram -> IO Result
runTestProgram
  ( TestProgram
      program
      args
      workingDir
      input
      exitCode
      checkStderr
      checkStdout
      timeout
    ) = do
    execFound <- findExecutable program

    case execFound of
      Nothing -> return $ execNotFoundFailure program
      Just progPath ->
        runProgram
          progPath
          args
          workingDir
          input
          exitCode
          checkStderr
          checkStdout
          timeout

instance IsTest TestProgram where
  run _ p _ = runTestProgram p

  testOptions = return []

runProgram
  :: String
  -- ^ Program name
  -> [String]
  -- ^ Program options
  -> Maybe FilePath
  -- ^ Optional working directory
  -> Maybe String
  -- ^ Input
  -> ExitCode
  -- ^ Expected exit code
  -> CheckOutput
  -- ^ A function to check whether the stderr is correct
  -> CheckOutput
  -- ^ A function to check whether the stdout is correct
  -> Int
  -- ^ Timeout in microseconds
  -> IO Result
runProgram program args workingDir input exitCode checkStderr checkStdout timeout = do
  (stdinH, stdoutH, stderrH, pid) <-
    runInteractiveProcess program args workingDir Nothing
  for_ ((++ "\n") <$> input) (hPutStr stdinH)
  let
    processAction = do
      stderr <- hGetContents stderrH
      stdout <- hGetContents stdoutH
      ecode <- stderr `deepseq` stdout `deepseq` waitForProcess pid
      return (ecode, stderr, stdout)
  result <- race (threadDelay timeout) processAction
  case result of
    Left () -> do
      terminateProcess pid
      ecode <- waitForProcess pid
      return $ exitFailure program args ecode "" "" "program timeout"
    Right (ecode, stderr, stdout) -> do
      let
        exitFailure' = exitFailure program args ecode stderr stdout
      let
        (stderrCorrect, stderrReason) = checkStderr stderr
      let
        (stdoutCorrect, stdoutReason) = checkStdout stdout
      let
        res
          | ecode /= exitCode =
              exitFailure'
                ("unexpected exit code " ++ show ecode ++ " expected " ++ show exitCode)
          | not stderrCorrect = exitFailure' ("stderr is incorrect\n" ++ stderrReason)
          | not stdoutCorrect = exitFailure' ("stdout is incorrect\n" ++ stdoutReason)
          | otherwise = success
      return res

-- | Indicates successful test
success :: Result
success = testPassed ""

-- | Indicates that program does not exist in the path
execNotFoundFailure :: String -> Result
execNotFoundFailure file =
  testFailed $ "Cannot locate program " ++ file ++ " in the PATH"

-- | Indicates that program failed with an error code
exitFailure
  :: String -> [String] -> ExitCode -> String -> String -> String -> Result
exitFailure file args code stderr stdout reason =
  testFailed $
    "program "
      ++ unwords (file : args)
      ++ " failed with code "
      ++ show code
      ++ "\nstderr was:\n"
      ++ stderr
      ++ "\nstdout was:\n"
      ++ stdout
      ++ "\nreason:\n"
      ++ reason
