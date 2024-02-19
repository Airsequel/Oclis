-- | CAUTION:
-- | THIS FILE WAS GENERATED BASED ON `oclis.ncl`.
-- | DO NOT EDIT MANUALLY!

module Oclis where

import Prelude
  ( Unit
  , bind
  , const
  , discard
  , pure
  , unit
  , (-)
  , (#)
  , (+)
  , (<#>)
  , (<>)
  , (>)
  , (||)
  , ($)
  )

import Ansi.Codes (Color(..))
import Ansi.Output (withGraphics, foreground)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (drop, find, fold, foldMap, foldl, head, length, replicate)
import Data.Bifunctor (lmap)
import Data.Eq ((==))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
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

errorAndExit :: String -> Effect (Result String String)
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

handleHelpOrVersion
  :: CliSpecRaw
  -> String
  -> (String -> Effect Unit)
  -> CliArgument
  -> Effect (Result String String)
  -> Effect (Result String String)
handleHelpOrVersion cliSpecRaw usageString logFunc arg otherwise =
  if
    arg == (CmdArg "help")
      || arg == (FlagLong "help")
      || arg == (FlagShort 'h') --
  then do
    logFunc usageString
    pure $ Ok usageString
  else if
    arg == (CmdArg "version")
      || arg == (FlagLong "version")
      || arg == (FlagShort 'v') --
  then do
    -- TODO: Only show version of subcommand (if available)
    let version = cliSpecRaw.version # fromMaybe "0"
    logFunc version
    pure $ Ok version

  else otherwise

-- | Recursively calls the command with the given arguments.
-- | The arguments include the command itself.
callCommand
  :: (String -> Effect Unit)
  -> Oclis
  -> String
  -> Array CliArgument
  -> (ExecutorContext -> Effect (Result String Unit))
  -> Effect (Result String String)
callCommand logFunc (Oclis cliSpecRaw) usageString args executor = do
  let
    mainCmd = args # head # case _ of
      Just (CmdArg cmdName) -> Just cmdName
      _ -> Nothing

  case args # drop 1 # head of
    Nothing -> do
      logFunc usageString
      setExitCode 1
      pure $ Error usageString

    Just arg ->
      handleHelpOrVersion cliSpecRaw usageString logFunc arg $ do
        case arg of
          (CmdArg cmdName) -> do
            let
              commandMb = cliSpecRaw.commands
                # fromMaybe []
                # find (\(Oclis cmd) -> cmd.name == cmdName)

            case commandMb of
              Nothing -> do
                let
                  errStr =
                    makeRed ("ERROR: Unknown command \"" <> cmdName <> "\"")
                      <> "\n\n"
                      <> usageString
                logFunc errStr
                setExitCode 1
                pure (Error errStr)

              Just cmdSpec@(Oclis cmdSpecRaw) -> do
                callCommand
                  logFunc
                  cmdSpec
                  (buildUsageString cmdSpecRaw)
                  (args # drop 1)
                  executor
          _ ->
            executor
              { command: mainCmd
              , usageString
              , arguments: args # drop 1
              }
              <#> (\res -> res <#> const "")

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
                then " [" <> arg.name <> moreThanOne <> "]"
                else " " <> arg.name
          )
        Nothing -> ""

    calcTotalLength :: _ -> Int
    calcTotalLength cmd =
      (Str.length cmd.name) +
        (Str.length $ buildArgsString cmd.arguments)

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

    lengthLongestCmd :: Int
    lengthLongestCmd =
      (cliSpecRaw.commands <> Just [ helpCmd, versionCmd ])
        # fromMaybe []
        # foldl
            ( \acc (Oclis cmd) -> do
                let totalLength = calcTotalLength cmd
                if acc > totalLength then acc else totalLength
            )
            0

    usageHeader = "USAGE: "
      <> cliSpecRaw.name
      <>
        ( if isJust cliSpecRaw.options --
          then " [options]"
          else ""
        )
      <>
        ( if isJust cliSpecRaw.commands --
          then " [command]"
          else buildArgsString cliSpecRaw.arguments
        )

  usageHeader
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
                    <> buildArgsString cmd.arguments
                    <> " "
                    <>
                      ( repeatString " "
                          (lengthLongestCmd - calcTotalLength cmd)
                      )
                    <> " "
                    <> cmd.description
                    <> "\n"
              )
      )

-- | Like `callCliAppWithOutput` but does print the result to stdout
callCliApp :: (ExecutorContext -> Effect (Result String Unit)) -> Effect Unit
callCliApp executor =
  callCliAppWithOutput true executor

-- | Convenience function to call the CLI app with the default spec and args.
-- | Use `callCliAppWith`` if you want to provide your own values.
-- | Does not print the result of the executor for testing purposes.
callCliAppWithOutput
  :: Boolean
  -> (ExecutorContext -> Effect (Result String Unit))
  -> Effect Unit
callCliAppWithOutput doesPrint executor =
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
      _ <- callCliAppWith cliSpec executor doesPrint arguments
      pure unit

callCliAppWith
  :: Oclis
  -> (ExecutorContext -> Effect (Result String Unit))
  -> Boolean
  -> Array String
  -> Effect (Result String String)
callCliAppWith cliSpec@(Oclis cliSpecRaw) executor doesPrint arguments = do
  let
    argsNoInterpreter = arguments # drop 1 -- Drop "node"
    logFunc =
      if doesPrint --
      then log
      else (\_ -> pure unit)

  if length argsNoInterpreter == 0 --
  then do
    errorAndExit "The CLI app must be called with at least one argument"
  else do
    let
      cliArgsMb = tokensToCliArguments cliSpec
        (tokenizeCliArguments argsNoInterpreter)

    case cliArgsMb of
      Error errMsg ->
        if doesPrint --
        then do
          setExitCode 1
          pure $ Error errMsg
        else
          errorAndExit errMsg
      Ok cliArgs -> do
        callCommand
          logFunc
          cliSpec
          (buildUsageString cliSpecRaw)
          cliArgs
          executor
