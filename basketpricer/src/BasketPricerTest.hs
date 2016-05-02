import Test.Hspec
import Test.QuickCheck
import BasketPricer ( Item(Apple, Banana), simplePrice )

main = hspec $ describe "simplePriceBasket" $ do
  it "works for empty basket" $ do
    simplePrice [] `shouldBe` 0.0

  it "works for baskets with some items" $ do
    simplePrice [Apple] `shouldBe` 0.6
    simplePrice [Apple, Apple] `shouldBe` 1.2
    simplePrice [Banana] `shouldBe` 0.3
    simplePrice [Banana, Banana] `shouldBe` 0.6
    simplePrice [Apple, Apple, Apple] `shouldSatisfy` (\x -> abs( x - 1.8 ) <= 0.001)

