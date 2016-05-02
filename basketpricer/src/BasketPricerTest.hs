import Test.Hspec
import Test.QuickCheck
import BasketPricer ( Item(Apple, Banana),
                      simplePrice,
                      buyOneGetOneFree,
                      buyTwoGetOneFree,
                      buyOneGetOneHalfPrice,
                     )

main = hspec $
 describe "simplePriceBasket" $ do
  it "works for empty basket" $ do
    simplePrice [] [] `shouldBe` 0.0

  it "works for baskets with some items, no promotions" $ do
    simplePrice [Apple] [] `shouldBe` 0.6
    simplePrice [Apple, Apple] [] `shouldBe` 1.2
    simplePrice [Banana] [] `shouldBe` 0.3
    simplePrice [Banana, Banana] [] `shouldBe` 0.6
    simplePrice [Apple, Apple, Apple] [] `shouldSatisfy` (\x -> abs( x - 1.8 ) <= 0.001)

  --- BOGOF
  describe "buyOneGetOneFree" $ do
      it "works for empty basket" $ do
        buyOneGetOneFree Apple [] `shouldBe` (0.0, [])

      it "works for baskets with two eligible items" $ do
        buyOneGetOneFree Apple [Apple, Apple] `shouldBe` (0.6, [Apple, Apple])

      it "works for baskets with two eligible items and other non eligible items" $ do
        buyOneGetOneFree Apple [Apple, Apple, Apple, Banana] `shouldBe` (0.6, [Apple, Apple])

      it "discount is applied once per invocation" $ do
        buyOneGetOneFree Apple [Apple, Apple, Apple, Apple] `shouldBe` (0.6, [Apple, Apple])

  describe "buyTwoGetOneFree" $ do
      it "works for empty basket" $ do
        buyTwoGetOneFree Apple [] `shouldBe` (0.0, [])

      it "works for baskets with  eligible items" $ do
        buyTwoGetOneFree Apple [Apple, Apple, Apple] `shouldBe` (0.6, [Apple, Apple, Apple])

      it "works for baskets with three eilgible items and other non eligible items" $ do
        buyTwoGetOneFree Apple [Apple, Apple, Apple, Banana] `shouldBe` (0.6, [Apple, Apple, Apple])

      it "discount is applied once per invocation" $ do
        buyTwoGetOneFree Apple [Apple, Apple, Apple, Apple, Apple, Apple] `shouldBe` (0.6, [Apple, Apple, Apple])

  describe "buyOneGetOneHalfPrice" $ do
      it "works for empty basket" $ do
        buyOneGetOneHalfPrice Apple [] `shouldBe` (0.0, [])

      it "works for baskets with  eligible items" $ do
        buyOneGetOneHalfPrice Apple [Apple, Apple, Apple] `shouldBe` (0.3, [Apple, Apple])

      it "works for baskets with two eilgible items and other non eligible items" $ do
        buyOneGetOneHalfPrice Apple [Apple, Apple, Banana] `shouldBe` (0.3, [Apple, Apple])

      it "discount is applied once per invocation" $ do
        buyOneGetOneHalfPrice Apple [Apple, Apple, Apple, Apple, Apple, Apple] `shouldBe` (0.3, [Apple, Apple])

  describe "simplePriceBasketWithBOGOFPromotions" $ do

      it "works for baskets with some items and BOGOF" $ do
        simplePrice [Apple, Apple] [buyOneGetOneFree Apple] `shouldBe` 0.6

      it "works for baskets with 2 item pairs and BOGOF on each" $ do
        simplePrice [Apple, Apple, Banana, Banana] [buyOneGetOneFree Apple, buyOneGetOneFree Banana] `shouldSatisfy` (\x -> abs( x - 0.9 ) <= 0.001)

      it "works for baskets with 2 items but BOGOF on different item" $ do
        simplePrice [Banana, Banana] [buyOneGetOneFree Apple] `shouldBe` 0.6

      it "works for baskets with 3 items pairs with BOGOF " $ do
        simplePrice [Banana, Banana, Banana, Banana, Banana, Banana] [buyOneGetOneFree Banana] `shouldSatisfy` (\x -> abs( x - 0.9 ) <= 0.001)

      it "works for baskets with 2 items pairs with BOGOF and one item does not qualify" $ do
        simplePrice [Apple, Apple, Apple, Apple, Apple] [buyOneGetOneFree Apple] `shouldSatisfy` (\x -> abs( x - 1.8 ) <= 0.001)


  describe "simplePriceBasketWithBOGOHPPromotions" $ do

      it "works for baskets with some items and BOGOHP" $ do
        simplePrice [Apple, Apple] [buyOneGetOneHalfPrice Apple] `shouldSatisfy` (\x -> abs( x - 0.9 ) <= 0.001)

      it "works for baskets with 2 item pairs and BOGOHP on each" $ do
        simplePrice [Apple, Apple, Banana, Banana] [buyOneGetOneHalfPrice Apple, buyOneGetOneHalfPrice Banana] `shouldSatisfy` (\x -> abs( x - 1.35 ) <= 0.001)

      it "works for baskets with 2 items but BOGOHP on different item" $ do
        simplePrice [Banana, Banana] [buyOneGetOneHalfPrice Apple] `shouldBe` 0.6

      it "works for baskets with 3 items pairs with BOGOHP " $ do
        simplePrice [Banana, Banana, Banana, Banana, Banana, Banana] [buyOneGetOneHalfPrice Banana] `shouldSatisfy` (\x -> abs( x - 1.35 ) <= 0.001)

      it "works for baskets with 2 items pairs with BOGOHP and one item does not qualify" $ do
        simplePrice [Apple, Apple, Apple, Apple, Apple] [buyOneGetOneHalfPrice Apple] `shouldSatisfy` (\x -> abs( x - 2.40 ) <= 0.001)

  describe "simplePriceBasketWith3For2Promotions" $ do

      it "works for baskets with some items and 3For2" $ do
        simplePrice [Apple, Apple, Apple] [buyTwoGetOneFree Apple] `shouldSatisfy` (\x -> abs( x - 1.2 ) <= 0.001)

      it "works for baskets with 2 item triplets and 3For2 on each" $ do
        simplePrice [Apple, Apple, Apple, Banana, Banana, Banana] [buyTwoGetOneFree Apple, buyTwoGetOneFree Banana] `shouldSatisfy` (\x -> abs( x - 1.80 ) <= 0.001)

      it "works for baskets with 3 items but 3For2 on different item" $ do
        simplePrice [Banana, Banana, Banana] [buyTwoGetOneFree Apple] `shouldSatisfy` (\x -> abs( x - 0.9 ) <= 0.001)

      it "works for baskets with 2 items pairs with BOGOHP " $ do
        simplePrice [Banana, Banana, Banana, Banana, Banana, Banana] [buyTwoGetOneFree Banana] `shouldSatisfy` (\x -> abs( x - 1.20 ) <= 0.001)

      it "works for baskets with 1 items triplet with 3For2 and two items do not qualify" $ do
        simplePrice [Apple, Apple, Apple, Apple, Apple] [buyTwoGetOneFree Apple] `shouldSatisfy` (\x -> abs( x - 2.40 ) <= 0.001)


  describe "simplePriceBasketWithDifferentCombinationsOfBOGOFBOGOHPAnd3For2Promotions" $ do

      it "works for baskets with no items " $ do
        simplePrice [] [buyTwoGetOneFree Apple, buyOneGetOneHalfPrice Apple, buyTwoGetOneFree Banana] `shouldBe` 0.0

      it "works for baskets with BOGOF on Apple and 3For2 on Banana" $ do
        simplePrice [Apple, Apple, Apple, Apple, Banana, Banana, Banana] [buyTwoGetOneFree Banana, buyOneGetOneFree Apple] `shouldSatisfy` (\x -> abs( x - 1.80 ) <= 0.001)

      it "works for baskets with BOGOF on Apple and BOGOHP on banana" $ do
        simplePrice [Apple, Apple, Banana, Banana] [buyOneGetOneFree Apple, buyOneGetOneHalfPrice Banana ] `shouldSatisfy` (\x -> abs( x - 1.05 ) <= 0.001)

      it "works for baskets with 2 pairs of BOGOF Banana and 3For2 Apples " $ do
        simplePrice [Banana, Banana, Banana, Banana, Apple, Apple, Apple] [buyTwoGetOneFree Apple, buyOneGetOneFree Banana] `shouldSatisfy` (\x -> abs( x - 1.80 ) <= 0.001)

      it "works for baskets with 1 items triplet with 3For2, One BOGOF Banana and three items do not qualify" $ do
        simplePrice [Apple, Apple, Apple, Apple, Apple, Banana, Banana, Banana] [buyTwoGetOneFree Apple, buyOneGetOneFree Banana] `shouldSatisfy` (\x -> abs( x - 3.00 ) <= 0.001)
