module TwainSpec where

import Web.Twain
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.String (fromString)
import Test.Hspec
import qualified Test.Hspec.Wai as Test

-- * Tests

spec :: Spec
spec = Test.with (pure app) $ do
  describe "GET" $ do
    describe "GET /" $ do
      it "responds with 200 & html 'hello world'" $
        Test.shouldRespondWith
          (Test.get "/")
          "Hello World!"
            { Test.matchStatus = 200
            , Test.matchHeaders = ["Content-Type" Test.<:> "text/html; charset=utf-8"]
            }

    describe "GET /json" $ do
      it "responds with Content-Type application/json" $
        Test.shouldRespondWith
          (Test.get "/json")
          "[true]"
            { Test.matchStatus = 200
            , Test.matchHeaders = ["Content-Type" Test.<:> "application/json; charset=utf-8"]
            }

    describe "GET /css" $ do
      it "responds with Content-Type text/css" $
        Test.shouldRespondWith
          (Test.get "/css")
          (fromString style)
            { Test.matchStatus = 200
            , Test.matchHeaders = ["Content-Type" Test.<:> "text/css; charset=utf-8"]
            }

    describe "GET /notfound" $ do
      it "responds with 404 / 'Not found...'" $
        Test.shouldRespondWith
          (Test.get "/notfound")
          "Not found..." { Test.matchStatus = 404 }

  describe "POST" $ do
    describe "POST /echo/James" $ do
      it "Without body - responds with 200 & 'Hello, James'" $
        Test.shouldRespondWith
          (Test.post "/echo/James" "")
          "Hello, James" { Test.matchStatus = 200 }

      it "With body - responds with 200 & 'Hello, Jameson'" $
        Test.shouldRespondWith
          (Test.postHtmlForm "/echo/James" [("bodyname", "Jameson")])
          "Hello, Jameson" { Test.matchStatus = 200 }

    describe "POST /echo/" $ do
      it "responds with 404 & 'Person not specified...' (next works correctly)" $
        Test.shouldRespondWith
          (Test.postHtmlForm "/echo/" [])
          "Person not specified..." { Test.matchStatus = 404 }

-- * App

app :: Application
app = foldr ($) (notFound missing) routes

routes :: [Middleware]
routes =
  [ get "/" index
  , get "/json" myjson
  , get "/css" (send $ css $ fromString style)
  , post "/echo/:name" echo
  , post "/echo/:name" missingPerson
  ]

index :: ResponderM a
index = send $ html "Hello World!"

myjson :: ResponderM a
myjson = send $ json [True]

style :: String
style = "body { width: 900px; margin: auto; }"

echo :: ResponderM a
echo = do
  name <- fmap (unwords . words) $ paramMaybe "bodyname" >>= maybe (param "name") pure
  when (null name) $ next
  send $ html $ "Hello, " <> fromString name

missing :: ResponderM a
missing = send $ html "Not found..."

missingPerson :: ResponderM a
missingPerson =
  send $
    raw status404 [("Content-Type", "text/html; charset=utf-8")] $
      "Person not specified..."
