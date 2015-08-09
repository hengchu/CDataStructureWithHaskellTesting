import MCLRBTree
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Data.List (sort)
import Unsafe.Coerce
import Foreign.C.Types
import ArbitraryInstances ()

import MCLList

treeInsertHelper :: MCLRBTreeH -> [CIntPtr] -> IO ()
treeInsertHelper _ [] = return ()
treeInsertHelper tree (x:xs) = do
  insertItemIntoRBTree tree x
  treeInsertHelper tree xs
  
listRetrieveHelper :: MCLListH -> IO [CIntPtr]
listRetrieveHelper list =
  listRetrieveHelper2 0 []
  where listRetrieveHelper2 n acc = do
          v <- listNthItem list n
          case v of
            (Just v') -> listRetrieveHelper2 (n+1) (v':acc)
            Nothing   -> return acc

main :: IO ()
main = quickCheckWith stdArgs { maxSize = 2000, maxSuccess = 1000 } $
  prop_newTreeShouldBeEmpty
  .&&. prop_treeShouldHaveRightNumberOfItems
  .&&. prop_treeShouldBeBalanced
  .&&. prop_emptyTreeShouldFailOnDelete
  .&&. prop_preOrderVisitShouldBeSorted

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

accumulator :: MCLVisitor
accumulator item userData = do
  let list = unsafeCoerce userData :: MCLListH
  listAppendFront list item

prop_preOrderVisitShouldBeSorted :: Property
prop_preOrderVisitShouldBeSorted  =
  monadicIO $ do
    let items = [265,30,121,181,388,425,435,256,461,2,3,118] 
    tree <- run newDefaultRBTree
    run $ treeInsertHelper tree items
    list <- run  newList
    run $ visitRBTreePreorder tree accumulator (unsafeCoerce list)
    retrievedItems <- run $ listRetrieveHelper list
    assert $ retrievedItems == sort items
