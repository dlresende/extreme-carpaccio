{-# LANGUAGE OverloadedStrings #-}

module App (app) where

import Network.HTTP.Types (hContentType, methodPost, status200, status404)
import Network.Wai

app :: Application
app request respond
  | requestMethod request == methodPost && pathInfo request == ["ping"] =
      respond $ responseLBS status200 [(hContentType, "text/plain; charset=utf-8")] "pong"
  | otherwise =
      respond $ responseLBS status404 [(hContentType, "text/plain; charset=utf-8")] "Not Found"
