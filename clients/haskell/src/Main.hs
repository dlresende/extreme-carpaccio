module Main (main) where

import Network.Wai.Handler.Warp (run)

import App (app)

main :: IO ()
main = run 3000 app
