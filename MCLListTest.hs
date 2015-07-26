import MCLList
import Test.QuickCheck
import Test.QuickCheck.Monadic

main :: IO ()
main = quickCheck prop_listAddItemWillIncrementNumItems

prop_listAddItemWillIncrementNumItems :: Int -> Property
prop_listAddItemWillIncrementNumItems item =
  monadicIO $ do
    list <- run newList
    oldSize <- run $ listNumItems list
    run $ listAppendFront list (fromIntegral item)
    newSize <- run $ listNumItems list
    assert (oldSize +1 == newSize)
