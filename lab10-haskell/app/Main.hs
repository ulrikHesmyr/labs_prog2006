module Main where 

import Happstack.Server
import Control.Concurrent.Async (concurrently)
import Control.Monad (msum)

main :: IO ()
main = simpleHTTP nullConf $ msum
  [ dir "hello" $ method GET >> ok "Hello, World!"
  , dir "greet" $ path $ \name -> do
      --setHeader "Content-Type" "application/json" >>
      ok $ "{\"greet\": \"Hello\", \"name\": \"" ++ name ++ "\"}"
  ]



