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
