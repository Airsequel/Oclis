module Oclis.App where

import GHC.Show (show)
import Protolude (IO, Maybe (..), Show, Text, pure)

import Oclis.Option (Option)


data App = App
  { name :: Text
  , version :: Maybe Text
  , description :: Maybe Text
  , options :: [Option]
  , run :: [Option] -> [Text] -> IO ()
  , commands :: [App]
  }


instance Show App where
  show _ = "<app>"


defaultApp :: App
defaultApp =
  App
    { name = "oclis"
    , version = Nothing
    , description = Nothing
    , options = []
    , run = \_ _ -> pure ()
    , commands = []
    }
