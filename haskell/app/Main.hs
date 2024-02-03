{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Protolude (
  Bool (..),
  IO,
  Maybe (..),
  pure,
  putText,
  when,
  ($),
  (&),
  (<>),
 )
import Protolude qualified as P

import Oclis as O
import Oclis.App as OA
import Oclis.Option as OO
import Text.RawString.QQ (r)


myCliApp :: OA.App
myCliApp =
  OA.defaultApp
    { name = "my-cli-app"
    , version = Just "1.0"
    , OA.description =
        Just
          [r|
            # My CLI App

            This is a CLI app that does something
          |]
    , options =
        [ OO.Option
            { long = Just "verbose"
            , short = Nothing
            , OO.description = Just "Enable verbose output"
            , argument = ArgNone
            , defaultValue = P.undefined -- Boolean False
            , required = False
            }
        , OO.Option
            { long = Just "log-file"
            , short = Just 'l'
            , OO.description = Just "Log file to write to"
            , argument = ArgOne "FILE"
            , defaultValue = P.undefined -- Boolean False
            , required = False
            }
        ]
    , run =
        \opts _args -> do
          P.putText "Hello, world!"

          when (opts & O.hasFlag "verbose") $ do
            P.putText "Verbose output enabled"

          case opts & O.getOptionVal "log-file" of
            Nothing -> P.putText "No log file specified"
            Just file -> P.putText $ "Writing logs to " <> file
    , commands = []
    }


main :: IO ()
main = do
  putText "Test"
  -- TODO: execute myCliApp args
  pure ()
