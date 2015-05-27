module Model
where

data Order = Order
    { prices :: [Double]
    , quantities :: [Integer]
    , country :: String
    , reduction :: String
    } deriving (Show)

data Quantity = Quantity
    { _total :: Double } deriving (Show,Eq)

computeTotal :: Order -> Quantity
computeTotal _ =
  -- You should replace this dummy value with your algorithm
  Quantity 1000.0
