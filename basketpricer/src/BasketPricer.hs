module BasketPricer ( Item(Apple, Banana), simplePrice ) where

data Item = Banana | Apple deriving (Show)
type Basket = [Item]
type Discount = (Double, [Item])

price Apple  = 0.6
price Banana = 0.3

simplePrice :: Basket -> Double
simplePrice = (foldl (+) 0.0 ).map (price)