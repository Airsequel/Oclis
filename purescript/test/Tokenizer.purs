module Test.Tokenizer where

import Prelude (Unit, pure, unit, ($), (#))

import Oclis (parseCliSpec, callCommand)
import Oclis.Parser (tokensToCliArguments)
import Oclis.Tokenizer (CliArgToken(..), tokenizeCliArguments)
import Oclis.Types
  ( CliArgPrim(..)
  , CliArgument(..)
  , Oclis(..)
  , emptyCliSpec
  , emptyCliSpecRaw
  )
import Control.Bind (discard)
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
import Data.Result (Result(..))
import Data.String (Pattern(..), split)
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail, shouldReturn)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

tokenizeCliStr :: String -> Array CliArgToken
tokenizeCliStr str =
  str
    # split (Pattern " ")
    # tokenizeCliArguments

tests :: Spec Unit
tests =
  describe "Tokenizer" do
    it "parses a CLI invocation" do
      (tokenizeCliStr "git")
        `shouldEqual` [ TextToken "git" ]

    it "parses a standalone flag (for subcommands)" do
      (tokenizeCliStr "--help")
        `shouldEqual` [ FlagLongToken "help" ]

    it "parses a CLI with an argument" do
      (tokenizeCliStr "ls dir")
        `shouldEqual` [ TextToken "ls", TextToken "dir" ]

    it "parses a CLI invocation with a long flag" do
      (tokenizeCliStr "git --version")
        `shouldEqual` [ TextToken "git", FlagLongToken "version" ]

    it "parses a CLI invocation with a short flag" do
      (tokenizeCliStr "git -a")
        `shouldEqual` [ TextToken "git", FlagShortToken 'a' ]

    it "parses a CLI invocation with several short flags" do
      (tokenizeCliStr "git -ab")
        `shouldEqual`
          [ TextToken "git", FlagShortToken 'a', FlagShortToken 'b' ]

    it "parses a CLI invocation with a long flag and an argument" do
      (tokenizeCliStr "git --verbose dir")
        `shouldEqual`
          [ TextToken "git"
          , FlagLongToken "verbose"
          , TextToken "dir"
          ]

    it "parses a CLI invocation with a long option" do
      (tokenizeCliStr "git --git-dir=dir")
        `shouldEqual`
          [ TextToken "git"
          , OptionLongToken "git-dir" (TextArg "dir")
          ]

    it "parses a CLI invocation with a short option" do
      (tokenizeCliStr "git -d=dir")
        `shouldEqual`
          [ TextToken "git"
          , OptionShortToken 'd' (TextArg "dir")
          ]
