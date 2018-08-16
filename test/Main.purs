module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "You should add some tests."

--  logShow (readShape "{\"tag\": \"Point\"}")
--  logShow (readShape "{\"tag\": \"Circle\", \"contents\": 100.0}")
--  logShow (readShape "{\"tag\": \"Rectangle\", \"contents\": [11.0, 12.0]}")
--  logShow (readShape "{\"tag\": \"NamedCircle\", \"contents\": [\"X\", 12.0]}")
--  logShow (readShape "{\"tag\": \"NamedRectangle\", \"contents\": [\"X\", 12.0, 33131.0]}")
