module Test.Executor where

import Prelude (Unit, pure, unit, ($))

import Oclis (callCommand)
import Oclis.Parser (tokensToCliArguments)
import Oclis.Tokenizer (tokenizeCliArguments)
import Oclis.Types (CliArgPrim(..), CliArgument(..), Oclis(..), emptyCliSpecRaw)
import Control.Bind (discard)
import Data.Maybe (Maybe(..))
import Data.Result (Result(..))
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail, shouldReturn)

tests :: Spec Unit
tests =
  describe "Execution" do
    describe "Help" do
      let
        cliSpec = Oclis emptyCliSpecRaw
        usageString = "Irrelevant"
        executor cmdName usageStr providedArgs = do
          cmdName `shouldEqual` "help"
          usageStr `shouldEqual` usageString
          providedArgs `shouldEqual` []
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
        executor cmdName usageStr providedArgs = do
          cmdName `shouldEqual` "help"
          usageStr `shouldEqual` usageString
          providedArgs `shouldEqual` []
          pure $ Ok unit

      it "shows help output for -v" do
        let
          toolArgs = [ "git", "-v" ]
          tokens = tokenizeCliArguments toolArgs

        case tokensToCliArguments cliSpec tokens of
          Error err -> fail err
          Ok cliArgs ->
            liftEffect (callCommand cliSpec usageString cliArgs executor)
              `shouldReturn` (Ok unit)

      it "shows help output for --version" do
        let
          toolArgs = [ "git", "--version" ]
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
        executor cmdName usageStr providedArgs = do
          cmdName `shouldEqual` "pull"
          usageStr `shouldEqual` usageString
          providedArgs `shouldEqual` [ (ValArg (TextArg "dir")) ]
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
        executor cmdName usageStr providedArgs = do
          cmdName `shouldEqual` "pull"
          usageStr `shouldEqual` usageString
          providedArgs `shouldEqual` [ (FlagLong "stats") ]
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
        executor cmdName usageStr providedArgs = do
          cmdName `shouldEqual` "pull"
          usageStr `shouldEqual` usageString
          providedArgs `shouldEqual` [ (OptionLong "output" (TextArg "dir")) ]
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
