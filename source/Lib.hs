module Lib where

import Protolude (Text, (<>))

sayHello :: Text -> Text
sayHello name =
  "Hello " <> name <> "!"
