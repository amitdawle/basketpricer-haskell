module BasketPricer ( Item(Apple, Banana),
                      simplePrice,
                      buyOneGetOneFree,
                      buyTwoGetOneFree,
                      buyOneGetOneHalfPrice
                      ) where
import Data.List

data Item = Banana | Apple deriving (Show, Eq)
type Basket = [Item]
type Discount = (Double, [Item])
type Promotion = Basket -> Discount
type Promotions = [Promotion]

price Apple  = 0.6
price Banana = 0.3

simplePrice :: Basket -> Promotions -> Double
simplePrice basket promotions = basketPrice - totalDiscount
                where totalDiscount = sum.map (fst.fst) $ [ applyPromotion p basket | p <- promotions   ]
                      basketPrice = (foldl (+) 0.0 ).map (price) $ basket

buyOneGetOneFree :: Item -> Basket -> Discount
buyOneGetOneFree item basket = multiBuyOffer item 2 price basket

buyOneGetOneHalfPrice :: Item -> Basket -> Discount
buyOneGetOneHalfPrice item basket = multiBuyOffer item 2 halfPrice basket
                      where halfPrice = (* 0.5).price

buyTwoGetOneFree :: Item -> Basket -> Discount
buyTwoGetOneFree item basket = multiBuyOffer item 3 price basket

multiBuyOffer :: Item -> Int ->  (Item -> Double) -> Basket -> Discount
multiBuyOffer item n p basket = if( length eligibleItems >= n ) then ((p item) , replicate n item) else (0.0, [])
                      where eligibleItems = filter ( == item) basket

applyPromotion :: Promotion -> Basket -> (Discount, Basket)
applyPromotion p [] = ((0.0, []), [] )
applyPromotion p basket = if( discount == 0.0) then ((0.0, []), newBasket)
                          else let
                              ((moreDiscount, moreDicountedItems),  updatedBasket) = applyPromotion p newBasket
                              totalDiscount = ( discount + moreDiscount , discountedItems ++ moreDicountedItems  )
                              in (totalDiscount, updatedBasket)
             where (discount, discountedItems) = p basket
                   newBasket = basket \\ discountedItems
