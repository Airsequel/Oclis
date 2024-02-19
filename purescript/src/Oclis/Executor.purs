-- | CAUTION:
-- | THIS FILE WAS GENERATED BASED ON `oclis.ncl`.
-- | DO NOT EDIT MANUALLY!

module Oclis where

import Prelude
  ( Unit
  , bind
  , discard
  , pure
  , unit
  , (#)
  , ($)
  , (-)
  , (<>)
  , (>)
  , (||)
  , (+)
  )

import Ansi.Codes (Color(..))
import Ansi.Output (withGraphics, foreground)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (drop, find, fold, foldMap, foldl, head, replicate)
import Data.Bifunctor (lmap)
import Data.Eq ((==))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Result (Result(..), fromEither)
import Data.String as Str
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Class.Console (log, error)
import Node.Process (argv, setExitCode)

import Oclis.Parser (tokensToCliArguments)
import Oclis.SpecEmbed (fileContent)
import Oclis.Tokenizer (tokenizeCliArguments)
import Oclis.Types

-- TODO: Automatically disable colors if not supported
makeRed :: String -> String
makeRed str =
  withGraphics (foreground Red) str

makeYellow :: String -> String
makeYellow str =
  withGraphics (foreground Yellow) str

errorAndExit :: String -> Effect (Result String Unit)
errorAndExit message = do
  error (makeRed message)
  setExitCode 1
  pure $ Error message

parseCliSpec :: String -> Result String Oclis
parseCliSpec cliSpecJsonStr = do
  let cliSpecRes = fromEither $ jsonParser cliSpecJsonStr

  case cliSpecRes of
    Error msg -> Error msg
    Ok cliSpecJson -> do
      cliSpecJson
        # decodeJson
        # (lmap printJsonDecodeError)
        # fromEither

type ExecutorContext =
  { usageString :: String
  , command :: Maybe String
  , arguments :: Array CliArgument
  }

callCommand
  :: Oclis
  -> String
  -> Array CliArgument
  -> (ExecutorContext -> Effect (Result String Unit))
  -> Effect (Result String Unit)
callCommand (Oclis cliSpec) usageString args executor = do
  case args # head of
    Nothing -> do
      log "No arguments provided"
      setExitCode 1
      pure (Error "No arguments provided")

    Just firstArg
      | firstArg == FlagShort 'h'
          || firstArg == FlagLong "help"
          || firstArg == CmdArg "help" -> do
          log usageString
          pure $ Ok unit

    Just firstArg
      | firstArg == FlagShort 'v'
          || firstArg == FlagLong "version"
          || firstArg == CmdArg "version" -> do
          log $ cliSpec.version # fromMaybe "0"
          pure $ Ok unit

    Just _mainCmd ->
      case args # drop 1 # head of
        Just arg
          | arg == (CmdArg "help")
              || arg == (FlagLong "help")
              || arg == (FlagShort 'h') -> do
              -- TODO: Only show help for subcommand
              log usageString
              pure $ Ok unit

        Just arg
          | arg == (CmdArg "version")
              || arg == (FlagLong "version")
              || arg == (FlagShort 'v') -> do
              -- TODO: Only show version of subcommand (if available)
              log $ cliSpec.version # fromMaybe "0"
              pure $ Ok unit

        Just (CmdArg cmdName) -> do
          let
            commandMb = cliSpec.commands
              # fromMaybe []
              # find (\(Oclis cmd) -> cmd.name == cmdName)
            providedArgs = args # drop 2

          case commandMb of
            Nothing -> do
              let
                errStr =
                  makeRed ("ERROR: Unknown command \"" <> cmdName <> "\"")
                    <> "\n\n"
                    <> usageString
              log errStr
              setExitCode 1
              pure (Error errStr)

            Just (Oclis _command) -> do
              executor
                { command: Just cmdName
                , usageString
                , arguments: providedArgs
                }

        Just _ -> executor
          { command: Nothing
          , usageString
          , arguments: args # drop 1
          }

        Nothing -> do
          log usageString
          setExitCode 1
          pure $ Error "No arguments provided"

-- | Function to repeat a string n times
repeatString :: String -> Int -> String
repeatString str n =
  fold $ replicate n str

buildUsageString :: CliSpecRaw -> String
buildUsageString cliSpecRaw = do
  let
    buildArgsString :: Maybe (Array Argument) -> String
    buildArgsString argsMb =
      case argsMb of
        Just args -> args # foldMap
          ( \arg ->
              let
                -- Not using `startsWith` here to avoid more dependencies
                moreThanOne =
                  case arg.type # Str.stripPrefix (Pattern "List-") of
                    Just _ -> "…"
                    Nothing -> ""
              in
                if arg.optional == Just true --
                then "[" <> arg.name <> moreThanOne <> "] "
                else arg.name <> " "
          )
        Nothing -> ""

    calcTotalLength :: _ -> Int
    calcTotalLength cmd =
      (Str.length cmd.name) +
        (Str.length $ buildArgsString cmd.arguments)

    lengthLongestCmd :: Int
    lengthLongestCmd =
      cliSpecRaw.commands
        # fromMaybe []
        # foldl
            ( \acc (Oclis cmd) -> do
                let totalLength = calcTotalLength cmd
                if acc > totalLength then acc else totalLength
            )
            0

    helpCmd = Oclis $
      ( emptyCliSpecRaw
          { name = "help"
          , description = "Show this help message"
          }
      )

    versionCmd = Oclis
      ( emptyCliSpecRaw
          { name = "version"
          , description = "Show version"
          }
      )

  "USAGE: " <> cliSpecRaw.name <> " <command> [options]"
    <> "\n\n"
    <> cliSpecRaw.description
    <> "\n\n"
    <> "COMMANDS:"
    <> "\n\n"
    <>
      ( cliSpecRaw.commands <> Just [ helpCmd, versionCmd ]
          # fromMaybe []
          # foldMap
              ( \(Oclis cmd) ->
                  cmd.name
                    <> " "
                    <> buildArgsString cmd.arguments
                    <>
                      ( repeatString " "
                          (lengthLongestCmd - calcTotalLength cmd)
                      )
                    <> " "
                    <> cmd.description
                    <> "\n"
              )
      )

-- | Convenience function to call the CLI app with the default spec and args.
-- | Use `callCliAppWith`` if you want to provide your own values.
callCliApp :: (ExecutorContext -> Effect (Result String Unit)) -> Effect Unit
callCliApp executor =
  case parseCliSpec fileContent of
    Error errMsg -> do
      error $
        "ERROR:\n"
          <> "The auto-generated CLI specification in SpecEmbed.purs "
          <> "could not be parsed.\n"
          <> "This should not be possible!\n"
          <> "Please make sure you didn't accidentally modify any Oclis files\n"
          <> "and report following error at "
          <> "https://github.com/Airsequel/Oclis/issues/new:\n"
          <> "\n"
          <> errMsg
      setExitCode 1
    Ok cliSpec -> do
      arguments <- argv
      _ <- callCliAppWith cliSpec executor arguments
      pure unit

callCliAppWith
  :: Oclis
  -> (ExecutorContext -> Effect (Result String Unit))
  -> Array String
  -> Effect (Result String Unit)
callCliAppWith cliSpec@(Oclis cliSpecRaw) executor arguments = do
  let
    argsNoInterpreter = arguments # drop 1 -- Drop "node"
    cliArgsMb =
      tokensToCliArguments
        cliSpec
        (tokenizeCliArguments argsNoInterpreter)

  case cliArgsMb of
    Error err -> errorAndExit err
    Ok cliArgs ->
      callCommand
        cliSpec
        (buildUsageString cliSpecRaw)
        cliArgs
        executor
