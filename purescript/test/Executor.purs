module Test.Executor where

import Prelude (Unit, bind, pure, unit, ($), (/=), (<>))

import Control.Bind (discard)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Result (Result(..))
import Data.String (replaceAll, Pattern(..), Replacement(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldReturn)
import Test.Spec.Assertions.String (shouldStartWith)

import Oclis (buildUsageString, callCliAppWith, callCliAppWithOutput)
import Oclis.Types (CliArgPrim(..), CliArgument(..), Oclis(..), emptyCliSpecRaw)

indentSubsequent :: Int -> String -> String
indentSubsequent indentation string =
  replaceAll
    (Pattern "\n")
    (Replacement ("\n" <> (" " `power` indentation)))
    string

-- | Better support for multi-line strings
testEqualityTo :: String -> String -> Result String String
testEqualityTo actual expected =
  if (actual /= expected) then Error
    $ indentSubsequent 2
    $ "=========== Actual ===========\n"
        <> replaceAll (Pattern "\n") (Replacement "|\n") actual
        <> "|\n"
        <> "========== Expected ==========\n"
        <> replaceAll (Pattern "\n") (Replacement "|\n") expected
        <> "|\n"
        <> "=============================="
        <> "\n\n"
  else Ok ""

shouldBeOk :: Result String String -> Aff Unit
shouldBeOk value = case value of
  Error error -> fail error
  Ok _ -> pure unit

shouldEqualString :: String -> String -> Aff Unit
shouldEqualString v1 v2 =
  case v1 `testEqualityTo` v2 of
    Error error -> fail error
    Ok _ -> (pure unit)

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

      liftEffect (callCliAppWithOutput false executor) `shouldReturn` unit

    describe "Help" do
      let
        cliSpec = Oclis emptyCliSpecRaw
        usageString = "Irrelevant"
        executor context = do
          context.command `shouldEqual` Just "help"
          context.usageString `shouldEqual` usageString
          context.arguments `shouldEqual` []
          pure $ Ok unit

      it "usage string contains sub-commands and their arguments" do
        let
          cliSpecRaw = emptyCliSpecRaw
            { name = "git"
            , description = "The git command"
            , version = Just "1.0.0"
            , commands = Just
                [ Oclis
                    ( emptyCliSpecRaw
                        { name = "add-file"
                        , description = "The add-file sub-command"
                        , arguments = Just
                            [ { name: "path"
                              , description: "Path to a file"
                              , type: "Text"
                              , optional: Nothing
                              , default: Nothing
                              }
                            ]
                        }
                    )
                , Oclis
                    ( emptyCliSpecRaw
                        { name = "commit"
                        , description = "The commit sub-command"
                        , arguments = Just
                            [ { name: "msg"
                              , description: "The commit message"
                              , type: "Text"
                              , optional: Just true
                              , default: Nothing
                              }
                            ]
                        }
                    )
                , Oclis
                    ( emptyCliSpecRaw
                        { name = "pull"
                        , description = "The pull sub-command"
                        , arguments = Just
                            [ { name: "dir"
                              , description: "Paths to directories"
                              , type: "List-Text"
                              , optional: Just true
                              , default: Nothing
                              }
                            ]
                        }
                    )
                ]
            }

        buildUsageString cliSpecRaw `shouldEqualString`
          ( "USAGE: git [command] [options] [args]\n"
              <> "\n"
              <> "The git command\n"
              <> "\n"
              <> "COMMANDS:\n"
              <> "\n"
              <> "add-file path  The add-file sub-command\n"
              <> "commit [msg]   The commit sub-command\n"
              <> "pull [dir…]    The pull sub-command\n"
              <> "help           Show this help message\n"
              <> "version        Show version\n"
          )

      it "shows help output for -h" do
        res <- liftEffect
          (callCliAppWith cliSpec executor false [ "sh", "git", "-h" ])
        shouldBeOk res

      it "shows help output for --help" do
        res <- liftEffect
          (callCliAppWith cliSpec executor false [ "sh", "git", "--help" ])
        shouldBeOk res

      it "shows help output for `help`" do
        res <- liftEffect
          (callCliAppWith cliSpec executor false [ "sh", "git", "help" ])
        shouldBeOk res

      it "shows help output for sub-commands" do
        let
          cliSpecRaw = emptyCliSpecRaw
            { name = "git"
            , commands = Just
                [ Oclis
                    ( emptyCliSpecRaw
                        { name = "pull"
                        , description = "The pull sub-command"
                        , arguments = Just
                            [ { name: "dir"
                              , description: "Paths to directories"
                              , type: "List-Text"
                              , optional: Just true
                              , default: Nothing
                              }
                            ]
                        }
                    )
                ]
            }

        res <- liftEffect $ callCliAppWith (Oclis cliSpecRaw) executor false
          [ "sh", "git", "pull", "--help" ]

        case res of
          Error err -> fail err
          Ok usageStr ->
            usageStr `shouldEqualString`
              -- TODO: Should be `git pull`
              ( "USAGE: pull [command] [options] [args]\n"
                  <> "\n"
                  <> "The pull sub-command\n"
                  <> "\n"
                  <> "COMMANDS:\n"
                  <> "\n"
                  <> "help  Show this help message\n"
                  <> "version  Show version\n"
              )

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
        liftEffect
          (callCliAppWith cliSpec executor false [ "sh", "git", "-v" ])
          `shouldReturn` (Ok "0")

      it "shows version output for --version" do
        liftEffect
          (callCliAppWith cliSpec executor false [ "sh", "git", "--version" ])
          `shouldReturn` (Ok "0")

      it "shows version output for `version`" do
        liftEffect
          (callCliAppWith cliSpec executor false [ "sh", "git", "version" ])
          `shouldReturn` (Ok "0")

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
        executor context = do
          context.command `shouldEqual` (Just "git")
          context.usageString `shouldStartWith`
            "USAGE: git [command] [options] [args]"
          context.arguments `shouldEqual`
            [ (FlagLong "color"), (FlagLong "debug") ]
          pure $ Ok unit

      liftEffect
        ( callCliAppWith cliSpec executor false
            [ "sh", "git", "--color", "--debug" ]
        )
        `shouldReturn` (Ok "")

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
        executor context = do
          context.command `shouldEqual` Just "pull"
          context.usageString `shouldStartWith`
            "USAGE: pull [command] [options] [args]"
          context.arguments `shouldEqual` [ (ValArg (TextArg "dir")) ]
          pure $ Ok unit

      liftEffect
        (callCliAppWith cliSpec executor false [ "sh", "git", "pull", "dir" ])
        `shouldReturn` (Ok "")

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
        executor context = do
          context.command `shouldEqual` Just "pull"
          context.usageString `shouldStartWith`
            "USAGE: pull [command] [options] [args]"
          context.arguments `shouldEqual` [ (FlagLong "stats") ]
          pure $ Ok unit

      liftEffect
        ( callCliAppWith cliSpec executor false
            [ "sh", "git", "pull", "--stats" ]
        )
        `shouldReturn` (Ok "")

    it "executes a sub-command with one option" do
      let
        cliSpec = Oclis
          emptyCliSpecRaw
            { name = "git"
            , commands = Just
                [ Oclis
                    ( emptyCliSpecRaw
                        { name = "pull"
                        , options = Just
                            [ { name: Just "remote"
                              , shortName: Nothing
                              , description: "Remote to pull from"
                              , argument: Just
                                  { name: "url"
                                  , description: "URL to remote"
                                  , type: "Text"
                                  , optional: Nothing
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

        executor context = do
          context.command `shouldEqual` Just "pull"
          context.usageString `shouldStartWith`
            "USAGE: pull [command] [options] [args]"
          context.arguments `shouldEqual`
            [ (OptionLong "remote" (TextArg "url")) ]
          pure $ Ok unit

      liftEffect
        ( callCliAppWith cliSpec executor false
            [ "sh", "git", "pull", "--remote", "url" ]
        )
        `shouldReturn` (Ok "")
