{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module MCLRBTree
(
  MCLRBTreeH
, newDefaultRBTree
, isRBTreeEmpty
, insertItemIntoRBTree
, numItemsRBTree
, depthRBTree
, deleteItemFromRBTree
, newRBTree
, MCLComparator
, MCLVisitor
, visitRBTreePreorder 
)
where

import Foreign
import Foreign.C.Types

newtype MCLRBTree = MCLRBTree (Ptr MCLRBTree)

foreign import ccall unsafe "mcl_rbtree.h mcl_rbtree_create_default"
  c_mcl_rbtree_create_default :: IO (Ptr MCLRBTree)

foreign import ccall safe "mcl_rbtree.h &mcl_rbtree_destroy"
  c_mcl_rbtree_destroy :: FunPtr ((Ptr MCLRBTree) -> IO ())

foreign import ccall safe "mcl_rbtree.h mcl_rbtree_empty"
  c_mcl_rbtree_empty :: (Ptr MCLRBTree) -> IO CInt

foreign import ccall safe "mcl_rbtree.h mcl_rbtree_insert"
  c_mcl_rbtree_insert :: (Ptr MCLRBTree) -> CIntPtr -> IO ()

foreign import ccall safe "mcl_rbtree.h mcl_rbtree_num_items"
  c_mcl_rbtree_num_items :: (Ptr MCLRBTree) -> IO Int

foreign import ccall safe "mcl_rbtree.h mcl_rbtree_depth"
  c_mcl_rbtree_depth :: (Ptr MCLRBTree) -> IO Int

foreign import ccall safe "mcl_rbtree.h mcl_rbtree_delete"
  c_mcl_rbtree_delete :: (Ptr MCLRBTree) -> CIntPtr -> IO Int

type MCLComparator = CIntPtr -> CIntPtr 
                             -> CIntPtr 
                             -> CUChar

foreign import ccall "wrapper"
  mk_mcl_comparator :: MCLComparator -> IO (FunPtr MCLComparator)

foreign import ccall safe "mcl_rbtree.h mcl_rbtree_create"
  c_mcl_rbtree_create :: (FunPtr MCLComparator) -> CIntPtr -> IO (Ptr MCLRBTree)

type MCLVisitor = CIntPtr -> CIntPtr -> IO ()

foreign import ccall "wrapper"
  mk_mcl_visitor :: MCLVisitor -> IO (FunPtr MCLVisitor)

foreign import ccall safe "mcl_rbtree.h mcl_rbtree_visit_pre_order"
  c_mcl_rbtree_visit_pre_order :: (Ptr MCLRBTree) -> (FunPtr MCLVisitor)
                                                  -> CIntPtr
                                                  -> IO ()

data MCLRBTreeH = MCLRBTreeH !(ForeignPtr MCLRBTree)
  deriving (Eq, Ord, Show)

newDefaultRBTree :: IO MCLRBTreeH
newDefaultRBTree = do
  tree <- c_mcl_rbtree_create_default
  treeFPtr <- newForeignPtr c_mcl_rbtree_destroy tree
  return $ MCLRBTreeH treeFPtr

newRBTree :: MCLComparator -> CIntPtr -> IO MCLRBTreeH
newRBTree comparator userData = do
  cmp <- mk_mcl_comparator comparator
  tree <- c_mcl_rbtree_create cmp userData
  treeFPtr <- newForeignPtr c_mcl_rbtree_destroy tree
  return $ MCLRBTreeH treeFPtr

isRBTreeEmpty :: MCLRBTreeH -> IO Bool
isRBTreeEmpty (MCLRBTreeH treePtr) = do
  withForeignPtr treePtr $ \ptr -> do 
    rc <- c_mcl_rbtree_empty ptr
    return $ rc > 0

insertItemIntoRBTree :: MCLRBTreeH -> CIntPtr -> IO ()
insertItemIntoRBTree (MCLRBTreeH treePtr) item = do
  withForeignPtr treePtr $ \ptr -> do
    c_mcl_rbtree_insert ptr item

numItemsRBTree :: MCLRBTreeH -> IO Int
numItemsRBTree (MCLRBTreeH treePtr) = do
  withForeignPtr treePtr c_mcl_rbtree_num_items

depthRBTree :: MCLRBTreeH -> IO Int
depthRBTree (MCLRBTreeH treePtr) = do
  withForeignPtr treePtr c_mcl_rbtree_depth

deleteItemFromRBTree :: MCLRBTreeH -> CIntPtr -> IO Bool
deleteItemFromRBTree (MCLRBTreeH treePtr) item = do
  withForeignPtr treePtr $ \ptr -> do
    rc <- c_mcl_rbtree_delete ptr item
    return $ rc == 0

visitRBTreePreorder :: MCLRBTreeH -> MCLVisitor -> CIntPtr -> IO ()
visitRBTreePreorder (MCLRBTreeH treePtr) visitor userData = do
  visitorFunPtr <- mk_mcl_visitor visitor
  withForeignPtr treePtr $ \ptr -> do
    c_mcl_rbtree_visit_pre_order ptr visitorFunPtr userData
