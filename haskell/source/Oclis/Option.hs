module Oclis.Option (
  Argument (..),
  Option (..),
  defaultOption,
) where

import Protolude (Bool (..), Char, Eq, Maybe (..), Show, Text)

import Data.Aeson (Value)


data Argument
  = ArgNone
  | ArgOne Text
  | ArgMany Text
  deriving (Show, Eq)


data Option = Option
  { long :: Maybe Text
  , short :: Maybe Char
  , description :: Maybe Text
  , argument :: Argument
  , required :: Bool
  , defaultValue :: Maybe Value
  }
  deriving (Show, Eq)


defaultOption :: Option
defaultOption =
  Option
    { long = Nothing
    , short = Nothing
    , description = Nothing
    , argument = ArgNone
    , required = False
    , defaultValue = Nothing
    }
