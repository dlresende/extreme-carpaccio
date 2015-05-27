{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import Data.Text
import Data.Monoid
import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Monad.Trans (liftIO)
import Web.Scotty
import Model

instance FromJSON Order where
  parseJSON (Object v) =
    Order <$>
    (v .: "prices") <*>
    (v .: "quantities") <*>
    (v .: "country") <*>
    (v .: "reduction")

data Feedback = Feedback
    { feedbackType :: Text
    , content      :: Text
    } deriving (Show)

instance FromJSON Feedback where
  parseJSON (Object v) =
    Feedback <$>
    (v .: "type") <*>
    (v .: "content")
  parseJSON _ = mzero

instance ToJSON Quantity where
  toJSON (Quantity total) = object [ "total" .= total]

main :: IO ()
main = scotty 8000 $ do

  get "/" $ do
    html "Hello extreme-carpaccio!"

  post "/feedback" $ do
    b <- body
    f <- case decode b of
      Just x -> return x
      Nothing -> fail "no feedback"

    liftIO $ print $ show (f :: Feedback)
    html "Thanx for this very usefull feedback"

  post "/order" $ do
    b <- body
    o <- case decode b of
      Just x -> return x
      Nothing -> fail "no order"

    liftIO $ print $ show (o :: Order)

    let answer = computeTotal o

    liftIO $ print $ show answer

    raw $ encode $ answer
