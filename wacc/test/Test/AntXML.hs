-- https://github.com/ocharles/tasty-ant-xml
-- Copyright 2013 Oliver Charles
-- All rights reserved.
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the author nor the names of his contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
-- THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
-- IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
-- ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{- | Run a 'Tasty.TestTree' and produce an XML file summarising the test results
in the same schema that would be produced by Apache Ant's JUnit test runner.
This schema can be intepreted by the Jenkins continuous integration server,
amongst other tools.
-}
module Test.AntXML (antXMLRunner, AntXMLPath (..)) where

import Control.Applicative (Const (Const))
import Control.Arrow (first)
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import qualified Data.Functor.Compose as Functor
import qualified Data.IntMap as IntMap
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo (..), Sum (..))
import Data.Proxy (Proxy (..))
import Data.Tagged (Tagged (..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Generics.Deriving.Monoid (mappenddefault, memptydefault)
import Numeric (showFFloat)
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Options as Tasty
import qualified Test.Tasty.Runners as Tasty
import qualified Text.XML.Light as XML

--------------------------------------------------------------------------------
newtype AntXMLPath = AntXMLPath FilePath
  deriving (Typeable)

instance Tasty.IsOption (Maybe AntXMLPath) where
  defaultValue = Nothing
  parseValue = Just . Just . AntXMLPath
  optionName = Tagged "xml"
  optionHelp = Tagged "A file path to store the test results in Ant-compatible XML"

--------------------------------------------------------------------------------
data Summary = Summary
  { summaryFailures :: Sum Int
  , summaryErrors :: Sum Int
  , summarySuccesses :: Sum Int
  , summaryIgnoreds :: Sum Int
  , xmlRenderer :: Endo [XML.Element]
  }
  deriving (Generic)

instance Monoid Summary where
  mempty = memptydefault

instance Semigroup Summary where
  (<>) = mappenddefault

--------------------------------------------------------------------------------

{- |

 To run tests using this ingredient, use 'Tasty.defaultMainWithIngredients',
 passing 'antXMLRunner' as one possible ingredient. This ingredient will run
 tests if you pass the @--xml@ command line option. For example,
 @--xml=junit.xml@ will run all the tests and generate @junit.xml@ as output.
-}
antXMLRunner :: Tasty.Ingredient
antXMLRunner = Tasty.TestReporter optionDescription runner
  where
    optionDescription = [Tasty.Option (Proxy :: Proxy (Maybe AntXMLPath))]
    runner options testTree = do
      AntXMLPath path <- Tasty.lookupOption options

      return $ \statusMap ->
        let
          timeDigits = 3
          showTime time = showFFloat (Just timeDigits) time ""

          runTest
            :: Tasty.OptionSet
            -> Tasty.TestName
            -> t
            -> Tasty.Traversal
                ( Functor.Compose
                    (Reader.ReaderT [String] (State.StateT IntMap.Key IO))
                    (Const Summary)
                )
          runTest _ testName _ = Tasty.Traversal $ Functor.Compose $ do
            i <- State.get
            groupNames <- Reader.ask

            summary <- liftIO $ STM.atomically $ do
              status <-
                STM.readTVar $
                  fromMaybe (error "Attempted to lookup test by index outside bounds") $
                    IntMap.lookup i statusMap

              let
                testCaseAttributes time =
                  map
                    (uncurry XML.Attr . first XML.unqual)
                    [ ("name", testName)
                    , ("time", showTime time)
                    , ("classname", intercalate "." (reverse groupNames))
                    ]

                mkSummary contents =
                  mempty
                    { xmlRenderer =
                        Endo
                          (XML.node (XML.unqual "testcase") contents :)
                    }

                mkSuccess time = (mkSummary (testCaseAttributes time)) {summarySuccesses = Sum 1}

                mkFailure time reason =
                  mkSummary
                    ( testCaseAttributes time
                    , XML.node (XML.unqual "failure") reason
                    )

                mkIgnored time reason =
                  (mkSummary (testCaseAttributes time, XML.node (XML.unqual "skipped") reason))
                    { summaryIgnoreds = Sum 1
                    }

              case status of
                -- If the test is done, generate XML for it
                Tasty.Done result
                  | Tasty.resultShortDescription result == "IGNORED" ->
                      pure (mkIgnored (Tasty.resultTime result) (Tasty.resultDescription result))
                  | Tasty.resultSuccessful result -> pure (mkSuccess (Tasty.resultTime result))
                  | otherwise ->
                      case resultException result of
                        Just e ->
                          pure $ (mkFailure (Tasty.resultTime result) (show e)) {summaryErrors = Sum 1}
                        Nothing ->
                          pure $
                            if resultTimedOut result
                              then (mkFailure (Tasty.resultTime result) "TimeOut") {summaryErrors = Sum 1}
                              else
                                (mkFailure (Tasty.resultTime result) (Tasty.resultDescription result))
                                  { summaryFailures = Sum 1
                                  }
                -- Otherwise the test has either not been started or is currently
                -- executing
                _ -> STM.retry

            Const summary <$ State.modify (+ 1)

          runGroup
            :: (Reader.MonadReader [String] f)
            => String
            -> Tasty.Traversal (Functor.Compose f (Const Summary))
            -> Tasty.Traversal (Functor.Compose f (Const Summary))
          runGroup groupName children = Tasty.Traversal $ Functor.Compose $ do
            Const soFar <-
              Reader.local (groupName :) $ Functor.getCompose $ Tasty.getTraversal children

            let
              grouped =
                XML.node
                  (XML.unqual "testsuite")
                  (
                    [ XML.Attr (XML.unqual "name") groupName
                    , XML.Attr
                        (XML.unqual "tests")
                        ( show
                            . getSum
                            . ( summaryFailures
                                  `mappend` summaryErrors
                                  `mappend` summarySuccesses
                                  `mappend` summaryIgnoreds
                              )
                            $ soFar
                        )
                    ]
                  , appEndo (xmlRenderer soFar) []
                  )

            pure $
              Const
                soFar
                  { xmlRenderer = Endo (grouped :)
                  }

          runGroup' _options = runGroup
        in
          do
            (Const summary, tests) <-
              flip State.runStateT 0 $
                flip Reader.runReaderT [] $
                  Functor.getCompose $
                    Tasty.getTraversal $
                      Tasty.foldTestTree
                        Tasty.trivialFold {Tasty.foldSingle = runTest, Tasty.foldGroup = runGroup'}
                        options
                        testTree

            return $ \elapsedTime -> do
              createPathDirIfMissing path
              writeFile path $
                XML.showTopElement $
                  XML.node
                    (XML.unqual "testsuites")
                    (
                      [ XML.Attr
                          (XML.unqual "errors")
                          (show . getSum . summaryErrors $ summary)
                      , XML.Attr
                          (XML.unqual "failures")
                          (show . getSum . summaryFailures $ summary)
                      , XML.Attr
                          (XML.unqual "skipped")
                          (show . getSum . summaryIgnoreds $ summary)
                      , XML.Attr (XML.unqual "tests") (show tests)
                      , XML.Attr (XML.unqual "time") (showTime elapsedTime)
                      ]
                    , appEndo (xmlRenderer summary) []
                    )

              return (getSum ((summaryFailures `mappend` summaryErrors) summary) == 0)

    resultException r =
      case Tasty.resultOutcome r of
        Tasty.Failure (Tasty.TestThrewException e) -> Just e
        _ -> Nothing

    resultTimedOut r =
      case Tasty.resultOutcome r of
        Tasty.Failure (Tasty.TestTimedOut _) -> True
        _ -> False

    createPathDirIfMissing path =
      canonicalizePath path
        >>= createDirectoryIfMissing True . takeDirectory
