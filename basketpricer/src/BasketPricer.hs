module BasketPricer ( Item(Apple, Banana),
                      simplePrice,
                      buyOneGetOneFree ) where

data Item = Banana | Apple deriving (Show, Eq)
type Basket = [Item]
type Discount = (Double, [Item])

price Apple  = 0.6
price Banana = 0.3

simplePrice :: Basket -> Double
simplePrice = (foldl (+) 0.0 ).map (price)

buyOneGetOneFree :: Item -> Basket -> Discount
buyOneGetOneFree item basket = if( length eligibleItems >= 2 ) then (price item , [item, item]) else (0.0, [])
                      where eligibleItems = filter ( == item) basket
