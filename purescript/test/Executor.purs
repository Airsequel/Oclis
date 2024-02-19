module Test.Executor where

import Prelude (Unit, pure, unit, ($))

import Control.Bind (discard)
import Data.Maybe (Maybe(..))
import Data.Result (Result(..))
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldReturn)

import Oclis (callCliApp, callCommand)
import Oclis.Parser (tokensToCliArguments)
import Oclis.Tokenizer (tokenizeCliArguments)
import Oclis.Types (CliArgPrim(..), CliArgument(..), Oclis(..), emptyCliSpecRaw)

tests :: Spec Unit
tests =
  describe "Execution" do
    it "executes a command with included spec" do
      let
        executor context = do
          context.command `shouldEqual` Nothing
          context.usageString `shouldEqual` "xxx"
          context.arguments `shouldEqual` []
          pure $ Ok unit

      liftEffect (callCliApp executor) `shouldReturn` unit

    describe "Help" do
      let
        cliSpec = Oclis emptyCliSpecRaw
        usageString = "Irrelevant"
        executor context = do
          context.command `shouldEqual` Just "help"
          context.usageString `shouldEqual` usageString
          context.arguments `shouldEqual` []
          pure $ Ok unit

      it "shows help output for -h" do
        let
          toolArgs = [ "git", "-h" ]
          tokens = tokenizeCliArguments toolArgs

        case tokensToCliArguments cliSpec tokens of
          Error err -> fail err
          Ok cliArgs ->
            liftEffect (callCommand cliSpec usageString cliArgs executor)
              `shouldReturn` (Ok unit)

      it "shows help output for --help" do
        let
          toolArgs = [ "git", "--help" ]
          tokens = tokenizeCliArguments toolArgs

        case tokensToCliArguments cliSpec tokens of
          Error err -> fail err
          Ok cliArgs ->
            liftEffect (callCommand cliSpec usageString cliArgs executor)
              `shouldReturn` (Ok unit)

      it "shows help output for `help`" do
        let
          toolArgs = [ "git", "help" ]
          tokens = tokenizeCliArguments toolArgs

        case tokensToCliArguments cliSpec tokens of
          Error err -> fail err
          Ok cliArgs ->
            liftEffect (callCommand cliSpec usageString cliArgs executor)
              `shouldReturn` (Ok unit)

    describe "Version" do
      let
        cliSpec = Oclis emptyCliSpecRaw
        usageString = "Irrelevant"
        executor context = do
          context.command `shouldEqual` Just "version"
          context.usageString `shouldEqual` usageString
          context.arguments `shouldEqual` []
          pure $ Ok unit

      it "shows version output for -v" do
        let
          toolArgs = [ "git", "-v" ]
          tokens = tokenizeCliArguments toolArgs

        case tokensToCliArguments cliSpec tokens of
          Error err -> fail err
          Ok cliArgs ->
            liftEffect (callCommand cliSpec usageString cliArgs executor)
              `shouldReturn` (Ok unit)

      it "shows version output for --version" do
        let
          toolArgs = [ "git", "--version" ]
          tokens = tokenizeCliArguments toolArgs

        case tokensToCliArguments cliSpec tokens of
          Error err -> fail err
          Ok cliArgs ->
            liftEffect (callCommand cliSpec usageString cliArgs executor)
              `shouldReturn` (Ok unit)

      it "shows version output for `version`" do
        let
          toolArgs = [ "git", "version" ]
          tokens = tokenizeCliArguments toolArgs

        case tokensToCliArguments cliSpec tokens of
          Error err -> fail err
          Ok cliArgs ->
            liftEffect (callCommand cliSpec usageString cliArgs executor)
              `shouldReturn` (Ok unit)

    it "executes a command with several flags" do
      let
        cliSpec = Oclis
          ( emptyCliSpecRaw
              { name = "git"
              , description = "The git command"
              , options = Just
                  [ { name: Just "color"
                    , shortName: Nothing
                    , description: "Set color of output"
                    , argument: Nothing
                    , optional: Nothing
                    , default: Nothing
                    }
                  , { name: Just "debug"
                    , shortName: Nothing
                    , description: "Activate debug mode"
                    , argument: Nothing
                    , optional: Nothing
                    , default: Nothing
                    }
                  ]
              }
          )
        toolArgs = [ "git", "--color", "--debug" ]
        tokens = tokenizeCliArguments toolArgs
        usageString = "Irrelevant"
        executor context = do
          context.command `shouldEqual` Nothing
          context.usageString `shouldEqual` usageString
          context.arguments `shouldEqual`
            [ (FlagLong "color"), (FlagLong "debug") ]
          pure $ Ok unit

      case tokensToCliArguments cliSpec tokens of
        Error err -> fail err
        Ok cliArgs ->
          liftEffect
            ( callCommand
                cliSpec
                usageString
                cliArgs
                executor
            ) `shouldReturn` (Ok unit)

    it "executes a sub-command with one argument" do
      let
        cliSpec = Oclis
          ( emptyCliSpecRaw
              { name = "git"
              , description = "The git command"
              , funcName = Just "runApp"
              , version = Just "1.0.0"
              , commands = Just
                  [ Oclis
                      ( emptyCliSpecRaw
                          { name = "pull"
                          , description = "The pull sub-command"
                          , funcName = Just "runPull"
                          , arguments = Just
                              [ { name: "dir"
                                , description: "Path to a directory"
                                , type: "Text"
                                , optional: Nothing
                                , default: Nothing
                                }
                              ]
                          }
                      )
                  ]
              }
          )
        toolArgs = [ "git", "pull", "dir" ]
        usageString = "Irrelevant"
        executor context = do
          context.command `shouldEqual` Just "pull"
          context.usageString `shouldEqual` usageString
          context.arguments `shouldEqual` [ (ValArg (TextArg "dir")) ]
          pure $ Ok unit

      case tokensToCliArguments cliSpec $ tokenizeCliArguments toolArgs of
        Error err -> fail err
        Ok cliArgs ->
          liftEffect
            ( callCommand
                cliSpec
                usageString
                cliArgs
                executor
            ) `shouldReturn` (Ok unit)

    it "executes a sub-command with one flag" do
      let
        cliSpec = Oclis
          ( emptyCliSpecRaw
              { name = "git"
              , description = "The git command"
              , funcName = Just "runApp"
              , version = Just "1.0.0"
              , commands = Just
                  [ Oclis
                      ( emptyCliSpecRaw
                          { name = "pull"
                          , description = "The pull sub-command"
                          , funcName = Just "runPull"
                          , options = Just
                              [ { name: Just "stats"
                                , shortName: Nothing
                                , description: "Statistics for pull"
                                , argument: Nothing
                                , optional: Nothing
                                , default: Nothing
                                }
                              ]
                          }
                      )
                  ]
              }
          )
        args = [ "git", "pull", "--stats" ]
        usageString = "Irrelevant"
        executor context = do
          context.command `shouldEqual` Just "pull"
          context.usageString `shouldEqual` usageString
          context.arguments `shouldEqual` [ (FlagLong "stats") ]
          pure $ Ok unit

      case (tokensToCliArguments cliSpec $ tokenizeCliArguments args) of
        Error err -> fail err
        Ok cliArgs ->
          liftEffect (callCommand cliSpec usageString cliArgs executor)
            `shouldReturn` (Ok unit)

    it "executes a sub-command with one option" do
      let
        cliSpec = Oclis
          ( emptyCliSpecRaw
              { name = "git"
              , description = "The git command"
              , funcName = Just "runApp"
              , version = Just "1.0.0"
              , commands = Just
                  [ Oclis
                      ( emptyCliSpecRaw
                          { name = "pull"
                          , description = "The pull sub-command"
                          , funcName = Just "runPull"
                          , options = Just
                              [ { name: Just "output"
                                , shortName: Nothing
                                , description: "Output directory"
                                , argument: Just
                                    { name: "dir"
                                    , description: "Path to a directory"
                                    , type: "Text"
                                    , optional: Nothing
                                    , default: Nothing
                                    }
                                , optional: Nothing
                                , default: Nothing
                                }
                              ]
                          , arguments = Just
                              [ { name: "dir"
                                , description: "Path to a directory"
                                , type: "Text"
                                , optional: Nothing
                                , default: Nothing
                                }
                              ]
                          }
                      )
                  ]
              }
          )
        toolArgs = [ "git", "pull", "--output", "dir" ]
        usageString = "Irrelevant"
        executor context = do
          context.command `shouldEqual` Just "pull"
          context.usageString `shouldEqual` usageString
          context.arguments `shouldEqual`
            [ (OptionLong "output" (TextArg "dir")) ]
          pure $ Ok unit

      case (tokensToCliArguments cliSpec $ tokenizeCliArguments toolArgs) of
        Error err -> fail err
        Ok cliArgs ->
          ( liftEffect $ callCommand
              cliSpec
              usageString
              cliArgs
              executor
          ) `shouldReturn` (Ok unit)
