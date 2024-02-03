module Oclis (
  module Oclis.App,
  module Oclis.Option,
  hasFlag,
  getOptionVal,
)
where

import Protolude (Bool, Maybe, Text)
import Protolude qualified as P

import Oclis.App
import Oclis.Option hiding (description)


hasFlag :: Text -> [Option] -> Bool
hasFlag = P.undefined


getOptionVal :: Text -> [Option] -> Maybe Text
getOptionVal = P.undefined
