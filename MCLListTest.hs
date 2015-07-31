import MCLList
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Maybe (isJust, fromJust)

import Foreign.C.Types
import Data.Word
import Data.List (sort)

listInsertHelper :: MCLListH -> [CIntPtr] -> IO ()
listInsertHelper _ [] = return ()
listInsertHelper list (x:xs) = do
  listAppendFront list (fromIntegral x)
  listInsertHelper list xs

listRetrieveHelper :: MCLListH -> IO [CIntPtr]
listRetrieveHelper list =
  listRetrieveHelper2 0 []
  where listRetrieveHelper2 n acc = do
          v <- listNthItem list n
          case v of
            (Just v') -> listRetrieveHelper2 (n+1) (v':acc)
            Nothing   -> return acc

genRandomList :: IO MCLListH
genRandomList = do
  list <- newList
  ints <- generate arbitrary
  listInsertHelper list ints
  return list

instance Arbitrary CIntPtr where
  arbitrary = do
    int <- arbitrary :: Gen Word
    return $ fromIntegral int

----------------------------------------------------------

main :: IO ()
main = quickCheckWith stdArgs{ maxSize=100, maxSuccess=50 } $ 
  prop_listAddItemWillIncrementNumItems
    .&&. prop_listShouldHaveCorrectLength
    .&&. prop_emptyListShouldNotHaveAnything
    .&&. prop_itemJustAddedShouldBeInList
    .&&. prop_emptyListShouldHaveNoHead
    .&&. prop_appendFrontItemShouldBeHead
    .&&. prop_numRemovesShouldBeEqualToSize
    .&&. prop_headShouldBeInList
    .&&. prop_valueJustAppendShouldExistWithPredicate
    .&&. prop_headShouldBeSameAsZerothItem
    .&&. prop_nthOutOfBoundsShouldFail
    .&&. prop_itemsInsertedShouldBeTheSame
    .&&. prop_itemsReversedShouldBeReverse
    .&&. prop_reverseIsSelfInverse
    .&&. prop_listSortShouldHaveSameLength
    .&&. prop_listSortShouldSort
    .&&. prop_listRevSortShouldRevSort

prop_listAddItemWillIncrementNumItems :: Int -> Property
prop_listAddItemWillIncrementNumItems item =
  monadicIO $ do
    list <- run genRandomList
    oldSize <- run $ listNumItems list
    run $ listAppendFront list (fromIntegral item)
    newSize <- run $ listNumItems list
    assert (oldSize +1 == newSize)

prop_listShouldHaveCorrectLength :: [CIntPtr] -> Property
prop_listShouldHaveCorrectLength items =
  let
    n = length items
  in monadicIO $ do
    list <- run genRandomList
    oldSize <- run $ listNumItems list
    run $ listInsertHelper list items
    size <- run $ listNumItems list
    assert (size == n + oldSize)

prop_emptyListShouldNotHaveAnything :: Int -> Property
prop_emptyListShouldNotHaveAnything item =
  monadicIO $ do
    list <- run newList
    empty <- run $ listIsEmpty list
    assert (empty)
    isIn <- run $ listInList list (fromIntegral item)
    assert (not isIn)

prop_itemJustAddedShouldBeInList :: Int -> Property
prop_itemJustAddedShouldBeInList item =
  monadicIO $ do
    list <- run genRandomList
    run $ listAppendFront list (fromIntegral item)
    isIn <- run $ listInList list (fromIntegral item)
    assert isIn

prop_emptyListShouldHaveNoHead :: Property
prop_emptyListShouldHaveNoHead = do
  monadicIO $ do
    list <- run newList
    h <- run $ listHead list
    assert (h == Nothing)

prop_appendFrontItemShouldBeHead :: CIntPtr -> Property
prop_appendFrontItemShouldBeHead item = do
  monadicIO $ do
    list <- run genRandomList
    run $ listAppendFront list (fromIntegral item)
    h <- run $ listHead list
    assert (Just item == h)

prop_numRemovesShouldBeEqualToSize :: Property
prop_numRemovesShouldBeEqualToSize = do
  monadicIO $ do
    list <- run genRandomList
    size <- run $ listNumItems list
    loop list size
    isEmpty <- run $ listIsEmpty list
    assert isEmpty
  where loop _ 0 = return ()
        loop l n = do
          h <- run $ listHead l
          assert (isJust h)
          rc <- run $ listRemove l (fromIntegral $ fromJust h)
          assert (rc == 0)
          loop l (n-1)

prop_headShouldBeInList :: Property
prop_headShouldBeInList = do
  monadicIO $ do
    list <- run genRandomList
    h <- run $ listHead list
    if (isJust h)
       then do rc <- run $ listInList list (fromIntegral $ fromJust h)
               assert rc
       else return ()

prop_valueJustAppendShouldExistWithPredicate :: CIntPtr -> Property
prop_valueJustAppendShouldExistWithPredicate item = do
  monadicIO $ do
    list <- run newList
    run $ listAppendFront list item
    let pred = \v _ -> if v == item then 1 else 0
    isIn <- run $ listInListIf list pred 0
    assert isIn

prop_headShouldBeSameAsZerothItem :: Property
prop_headShouldBeSameAsZerothItem = do
  monadicIO $ do
    list <- run genRandomList
    head <- run $ listHead list
    zeroth <- run $ listNthItem list 0
    assert (head == zeroth)

prop_nthOutOfBoundsShouldFail :: Property
prop_nthOutOfBoundsShouldFail = do
  monadicIO $ do
    list <- run genRandomList
    size <- run $ listNumItems list
    outOfBoundsN <- pick $ arbitrary
    pre (outOfBoundsN >= size)
    v <- run $ listNthItem list outOfBoundsN
    assert (v == Nothing)

prop_itemsInsertedShouldBeTheSame :: [CIntPtr] -> Property
prop_itemsInsertedShouldBeTheSame items = do
  monadicIO $ do
    list <- run newList
    run $ listInsertHelper list items
    itemsRetrieved <- run $ listRetrieveHelper list
    assert (items == itemsRetrieved)

prop_itemsReversedShouldBeReverse :: [CIntPtr] -> Property
prop_itemsReversedShouldBeReverse items = do
  monadicIO $ do
    list <- run newList
    run $ listInsertHelper list items
    run $ listReverse list
    itemsRetrieved <- run $ listRetrieveHelper list
    assert (reverse items == itemsRetrieved)

prop_reverseIsSelfInverse :: [CIntPtr] -> Property
prop_reverseIsSelfInverse items = do
  monadicIO $ do
    list <- run newList
    run $ listInsertHelper list items
    run $ listReverse list
    run $ listReverse list
    itemsRetrieved <- run $ listRetrieveHelper list
    assert (itemsRetrieved == items)

prop_listSortShouldHaveSameLength :: Property
prop_listSortShouldHaveSameLength = do
  monadicIO $ do
    list <- run genRandomList
    oldSize <- run $ listNumItems list
    run $ listReverse list
    newSize <- run $ listNumItems list
    assert (oldSize == newSize)
    
prop_listSortShouldSort :: [CIntPtr] -> Property
prop_listSortShouldSort items = do
  monadicIO $ do
    list <- run newList
    run $ listInsertHelper list items
    run $ listSort list
    itemsRetrieved <- run $ listRetrieveHelper list
    assert (reverse itemsRetrieved == sort items)

prop_listRevSortShouldRevSort :: [CIntPtr] -> Property
prop_listRevSortShouldRevSort items = do
  monadicIO $ do
    list <- run newList
    run $ listInsertHelper list items
    run $ listSortWith list (\a b _ -> if a > b then -1 else 1) 0
    itemsRetrieved <- run $ listRetrieveHelper list
    assert (itemsRetrieved == sort items)
