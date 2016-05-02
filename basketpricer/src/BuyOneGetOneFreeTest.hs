import Test.Hspec
import Test.QuickCheck
import BasketPricer ( Item(Apple, Banana), buyOneGetOneFree )

main = hspec $ describe "buyOneGetOneFree" $ do
  it "works for empty basket" $ do
    buyOneGetOneFree Apple [] `shouldBe` (0.0, [])

  it "works for baskets with two eilgible items" $ do
    buyOneGetOneFree Apple [Apple, Apple] `shouldBe` (0.6, [Apple, Apple])

  it "works for baskets with two eilgible items and other non eligible items" $ do
    buyOneGetOneFree Apple [Apple, Apple, Apple, Banana] `shouldBe` (0.6, [Apple, Apple])

  it "discount is applied once per invocation" $ do
    buyOneGetOneFree Apple [Apple, Apple, Apple, Apple] `shouldBe` (0.6, [Apple, Apple])
--    simplePrice [Apple, Apple] `shouldBe` 1.2
--    simplePrice [Banana] `shouldBe` 0.3
--    simplePrice [Banana, Banana] `shouldBe` 0.6
--    simplePrice [Apple, Apple, Apple] `shouldSatisfy` (\x -> abs( x - 1.8 ) <= 0.001)

