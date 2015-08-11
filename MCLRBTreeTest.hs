import MCLRBTree
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Data.List (sort, find)
import Data.Maybe (isJust)
import Foreign.C.Types
import ArbitraryInstances ()

main :: IO ()
main = quickCheckWith stdArgs { maxSize = 2000, maxSuccess = 3000 } $
  prop_newTreeShouldBeEmpty
  .&&. prop_treeShouldHaveRightNumberOfItems
  .&&. prop_treeShouldBeBalanced
  .&&. prop_emptyTreeShouldFailOnDelete
  .&&. prop_findIfShouldWork
  .&&. prop_findShouldWork
  -- .&&. prop_preOrderVisitShouldBeSorted

prop_newTreeShouldBeEmpty :: Property
prop_newTreeShouldBeEmpty = 
  monadicIO $ do
    tree <- run newDefaultRBTree
    isEmpty <- run $ isRBTreeEmpty tree
    assert isEmpty

prop_treeShouldHaveRightNumberOfItems :: [CIntPtr] -> Property
prop_treeShouldHaveRightNumberOfItems items =
  monadicIO $ do
    tree <- run newDefaultRBTree
    run $ treeInsertHelper tree items
    num <- run $ numItemsRBTree tree
    assert (num == length items)

prop_treeShouldBeBalanced :: [CIntPtr] -> Property
prop_treeShouldBeBalanced items =
  monadicIO $ do
    tree <- run newDefaultRBTree
    run $ treeInsertHelper tree items
    depth <- run $ depthRBTree tree
    let numItems  = (fromIntegral $ length items) :: Double
    assert $ fromIntegral depth <= (2 * logBase 2 (numItems+1))

prop_emptyTreeShouldFailOnDelete :: CIntPtr -> Property
prop_emptyTreeShouldFailOnDelete item =
  monadicIO $ do
    tree <- run newDefaultRBTree
    rc <- run $ deleteItemFromRBTree tree item
    assert $ not rc

{-
prop_preOrderVisitShouldBeSorted :: [CIntPtr] -> Property
prop_preOrderVisitShouldBeSorted items =
  monadicIO $ do
    tree <- run newDefaultRBTree
    run $ treeInsertHelper tree items
    list <- run  newList
    run $ visitRBTreePreorder tree accumulator (unsafeCoerce list)
    retrievedItems <- run $ listRetrieveHelper list
    assert $ retrievedItems == sort items
-}
 
prop_findIfShouldWork :: [CIntPtr] -> (CIntPtr -> Bool) -> Property
prop_findIfShouldWork items predicateH =
  monadicIO $ do
    let firstMatch = find predicateH $ sort items
    pre (isJust firstMatch)
    let cPredicate a _ = if predicateH a then 1 else 0
    tree <- run newDefaultRBTree
    run $ treeInsertHelper tree items
    rc <- run $ findWithPredicateInRBTree tree cPredicate 0
    assert rc

prop_findShouldWork :: [CIntPtr] -> CIntPtr -> Property
prop_findShouldWork items itemToFind =
  monadicIO $ do
    let inList = itemToFind `elem` items
    tree <- run newDefaultRBTree
    run $ treeInsertHelper tree items
    rc <- run $ findItemInRBTree tree itemToFind
    assert $ rc == inList
