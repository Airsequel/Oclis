module Test.Parser where

import Prelude (Unit)

import Control.Bind (discard)
import Data.Maybe (Maybe(..))
import Data.Result (Result(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

import Oclis (parseCliSpec)
import Oclis.Parser (tokensToCliArguments)
import Oclis.Tokenizer (CliArgToken(..))
import Oclis.Types
  ( CliArgPrim(..)
  , CliArgument(..)
  , Oclis(..)
  , emptyCliSpecRaw
  )
import Test.Tokenizer (tokenizeCliStr)

tests :: Spec Unit
tests =
  describe "Spec Parser" do
    let
      cliSpecRaw =
        emptyCliSpecRaw
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

    it "parses tokens for a command with only one flag" do
      let
        oneFlagSpec = Oclis
          ( emptyCliSpecRaw
              { name = "git"
              , options =
                  Just
                    [ { name: Just "help"
                      , shortName: Just "h"
                      , description: "Show help"
                      , argument: Nothing
                      , optional: Nothing
                      , default: Nothing
                      }
                    ]
              }
          )
        tokens = [ TextToken "git", FlagShortToken 'h' ]

      case tokensToCliArguments oneFlagSpec tokens of
        Error err -> fail err
        Ok cliArgs -> cliArgs `shouldEqual` [ CmdArg "git", FlagShort 'h' ]

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
        Ok parsedCliSpec -> parsedCliSpec `shouldEqual` (Oclis cliSpecRaw)

    it "correctly detects a subcommand with one argument" do
      let
        cliSpecWithFlag = Oclis
          ( cliSpecRaw
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
        cliSpecWithFlag = Oclis
          ( cliSpecRaw
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
        cliSpecWithFlag = Oclis
          cliSpecRaw
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
        cliSpecWithFlag = Oclis
          emptyCliSpecRaw
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
