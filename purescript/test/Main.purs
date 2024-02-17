module Test.Main where

import Prelude (Unit, ($))

import Control.Bind (discard)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Test.Executor as Test.Executor
import Test.Parser as Test.Parser
import Test.Tokenizer as Test.Tokenizer

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Oclis" do
    Test.Tokenizer.tests
    Test.Parser.tests
    Test.Executor.tests
