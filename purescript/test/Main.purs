module Test.Main where

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
tests = do
  describe "Oclis" do
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

    describe "Spec Parser" do
      let
        cliSpec :: Oclis
        cliSpec = Oclis
          ( emptyCliSpecRaw
              { name = "git"
              , description = "The git command"
              , funcName = Just "runApp"
              , version = Just "1.0.0"
              , commands = Just
                  [ Oclis
                      ( emptyCliSpecRaw
                          { name = "commit"
                          , description = "The commit sub-command"
                          , funcName = Just "runCommit"
                          , arguments = Just
                              [ { name: "pathspec"
                                , description: "File to commit"
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

      it "parses a full CLI spec" do
        let
          cliSpecJson =
            """
              { "name": "git",
                "description": "The git command",
                "funcName": "runApp",
                "version": "1.0.0",
                "commands": [
                  { "name": "commit",
                    "description": "The commit sub-command",
                    "funcName": "runCommit",
                    "arguments": [
                      { "name": "pathspec",
                        "description": "File to commit",
                        "type": "Text"
                      }
                    ]
                  }
                ]
              }
            """

        case parseCliSpec cliSpecJson of
          Error err -> fail err
          Ok parsedCliSpec -> parsedCliSpec `shouldEqual` cliSpec

      it "correctly detects a subcommand with one argument" do
        let
          cliSpecWithFlag :: Oclis
          cliSpecWithFlag = cliSpec # over Oclis
            ( \spec -> spec
                { commands = Just
                    [ Oclis
                        ( emptyCliSpecRaw
                            { name = "pull"
                            , description = "The pull sub-command"
                            , funcName = Just "runPull"
                            , arguments = Just
                                [ { name: "repository"
                                  , description: "Name of the repository"
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
          tokens = tokenizeCliStr "git pull origin"

        tokens `shouldEqual`
          [ TextToken "git"
          , TextToken "pull"
          , TextToken "origin"
          ]
        (tokensToCliArguments cliSpecWithFlag tokens)
          `shouldEqual`
            Ok
              [ CmdArg "git"
              , CmdArg "pull"
              , ValArg (TextArg "origin")
              ]

      it "correctly detects a subcommand with one long flag and one argument" do
        let
          cliSpecWithFlag :: Oclis
          cliSpecWithFlag = cliSpec # over Oclis
            ( \spec -> spec
                { commands = Just
                    [ Oclis
                        ( emptyCliSpecRaw
                            { name = "pull"
                            , description = "The pull sub-command"
                            , funcName = Just "runPull"
                            , options = Just
                                [ { name: Just "progress"
                                  , shortName: Nothing
                                  , description: "Show progress"
                                  , argument: Nothing
                                  , optional: Nothing
                                  , default: Nothing
                                  }
                                ]
                            , arguments = Just
                                [ { name: "repository"
                                  , description: "Name of the repository"
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
          tokens = tokenizeCliStr "git pull --progress origin"

        tokens `shouldEqual`
          [ TextToken "git"
          , TextToken "pull"
          , FlagLongToken "progress"
          , TextToken "origin"
          ]
        (tokensToCliArguments cliSpecWithFlag tokens)
          `shouldEqual`
            Ok
              [ CmdArg "git"
              , CmdArg "pull"
              , FlagLong "progress"
              , ValArg (TextArg "origin")
              ]

      it "redefines a long flag with a value to a long option" do
        let
          cliSpecWithFlag :: Oclis
          cliSpecWithFlag = cliSpec # over Oclis
            ( \spec -> spec
                { commands = Just
                    [ Oclis
                        ( emptyCliSpecRaw
                            { name = "pull"
                            , description = "The pull sub-command"
                            , funcName = Just "runPull"
                            , options = Just
                                [ { name: Just "strategy"
                                  , shortName: Nothing
                                  , description:
                                      "Set the preferred merge strategy"
                                  , argument: Just
                                      { name: "strategy"
                                      , description: "Strategy to use"
                                      , type: "Text"
                                      , optional: Just true
                                      , default: Nothing
                                      }
                                  , optional: Nothing
                                  , default: Nothing
                                  }
                                ]
                            }
                        )
                    ]
                }
            )
          tokens = tokenizeCliStr "git pull --strategy recursive"

        tokens `shouldEqual`
          [ TextToken "git"
          , TextToken "pull"
          , FlagLongToken "strategy"
          , TextToken "recursive"
          ]
        (tokensToCliArguments cliSpecWithFlag tokens)
          `shouldEqual`
            Ok
              [ CmdArg "git"
              , CmdArg "pull"
              , OptionLong "strategy" (TextArg "recursive")
              ]

      it "verifies number of args for variable number of allowed args" do
        let
          cliSpecWithFlag :: Oclis
          cliSpecWithFlag = emptyCliSpec # over Oclis
            ( \spec -> spec
                { name = "ls"
                , arguments = Just
                    [ { name: "file"
                      , description: "File to list"
                      , type: "Text"
                      , optional: Just false
                      , default: Nothing
                      }
                    , { name: "file"
                      , description: "Additional files to list"
                      , type: "List-Text"
                      , optional: Just true
                      , default: Nothing
                      }
                    ]
                }
            )

        let tokensOne = tokenizeCliStr "ls file1"
        (tokensToCliArguments cliSpecWithFlag tokensOne)
          `shouldEqual`
            Ok
              [ CmdArg "ls"
              , ValArg (TextArg "file1")
              ]

        let tokensTwo = tokenizeCliStr "ls file1 file2"
        (tokensToCliArguments cliSpecWithFlag tokensTwo)
          `shouldEqual`
            Ok
              [ CmdArg "ls"
              , ValArg (TextArg "file1")
              , ValArgList [ TextArg "file2" ]
              ]

        let tokensThree = tokenizeCliStr "ls file1 file2 file3"
        (tokensToCliArguments cliSpecWithFlag tokensThree)
          `shouldEqual`
            Ok
              [ CmdArg "ls"
              , ValArg (TextArg "file1")
              , ValArgList [ TextArg "file2", TextArg "file3" ]
              ]

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

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  tests
