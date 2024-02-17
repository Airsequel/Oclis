module Test.Parser where

import Prelude (Unit, (#))

import Control.Bind (discard)
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
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
  , emptyCliSpec
  , emptyCliSpecRaw
  )
import Test.Tokenizer (tokenizeCliStr)

tests :: Spec Unit
tests =
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
