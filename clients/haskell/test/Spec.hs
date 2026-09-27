{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types (status200)
import Network.Wai (Request (pathInfo, requestMethod))
import Network.Wai.Test
import Test.Hspec

import App (app)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "POST /ping" $
  it "responds with pong" $ do
    let pingRequest = defaultRequest {requestMethod = "POST", pathInfo = ["ping"]}
    response <- liftIO $ runSession (request pingRequest) app
    simpleStatus response `shouldBe` status200
    simpleBody response `shouldBe` "pong"
