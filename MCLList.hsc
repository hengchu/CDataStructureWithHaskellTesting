{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module MCLList
( MCLListH
, newList
, listNumItems
, listAppendFront
, listRemove
, listIsEmpty
, listInList
, listHead
, listInListIf
, listNthItem
, listReverse
, listSort
, listSortWith
)
where

import Foreign
import Foreign.Ptr
import Foreign.C.Types

#include <mcl_list.h>

newtype MCLList = MCLList (Ptr MCLList)

foreign import ccall unsafe "mcl_list.h mcl_list_create"
  c_mcl_list_create :: IO (Ptr MCLList)

foreign import ccall unsafe "mcl_list.h &mcl_list_destroy"
  c_mcl_list_destroy :: FunPtr ((Ptr MCLList) -> IO ())

foreign import ccall unsafe "mcl_list.h mcl_list_num_items"
  c_mcl_list_num_items :: (Ptr MCLList) -> IO Int

foreign import ccall unsafe "mcl_list.h mcl_list_append_front"
  c_mcl_list_append_front :: (Ptr MCLList) -> CIntPtr -> IO ()

foreign import ccall unsafe "mcl_list.h mcl_list_remove"
  c_mcl_list_remove :: (Ptr MCLList) -> CIntPtr -> IO Int

foreign import ccall unsafe "mcl_list.h mcl_list_empty"
  c_mcl_list_empty :: (Ptr MCLList) -> IO Int

foreign import ccall unsafe "mcl_list.h mcl_list_in_list"
  c_mcl_list_in_list :: (Ptr MCLList) -> CIntPtr -> IO Int

type MCLPred = CIntPtr -> CIntPtr -> CUChar

foreign import ccall "wrapper"
  mk_mcl_pred :: MCLPred -> IO (FunPtr MCLPred)

foreign import ccall safe "mcl_list.h mcl_list_in_list_if"
  c_mcl_list_in_list_if :: (Ptr MCLList) -> (FunPtr MCLPred) -> CIntPtr -> IO Int

foreign import ccall unsafe "mcl_list.h mcl_list_head"
  c_mcl_list_head :: (Ptr MCLList) -> (Ptr CIntPtr) -> IO Int

foreign import ccall unsafe "mcl_list.h mcl_list_nth_item"
  c_mcl_list_nth_item :: (Ptr MCLList) -> CInt -> (Ptr CIntPtr) -> IO Int

foreign import ccall unsafe "mcl_list.h mcl_list_reverse"
  c_mcl_list_reverse :: (Ptr MCLList) -> IO ()

foreign import ccall unsafe "mcl_list.h mcl_list_sort"
  c_mcl_list_sort :: (Ptr MCLList) -> IO ()

type MCLCmp = CIntPtr -> CIntPtr -> CIntPtr -> CInt

foreign import ccall safe "mcl_list.h mcl_list_sort_with"
  c_mcl_list_sort_with :: (Ptr MCLList) -> (FunPtr MCLCmp) -> CIntPtr -> IO ()

foreign import ccall "wrapper"
  mk_mcl_cmp :: MCLCmp -> IO (FunPtr MCLCmp)
  

data MCLListH = MCLListH !(ForeignPtr MCLList)
  deriving (Eq, Ord, Show)

newList :: IO MCLListH
newList = do
  list <- c_mcl_list_create
  listFPtr <- newForeignPtr c_mcl_list_destroy list
  return $ MCLListH listFPtr

listNumItems :: MCLListH -> IO Int
listNumItems (MCLListH listPtr) =
  withForeignPtr listPtr c_mcl_list_num_items

listAppendFront :: MCLListH -> CIntPtr -> IO ()
listAppendFront (MCLListH listPtr) item = do
  withForeignPtr listPtr $ \ptr -> 
    c_mcl_list_append_front ptr item

listRemove :: MCLListH -> CIntPtr -> IO Int
listRemove (MCLListH listPtr) item =
  withForeignPtr listPtr $ \ptr ->
    c_mcl_list_remove ptr item

listIsEmpty :: MCLListH -> IO Bool
listIsEmpty (MCLListH listPtr) = do
  rc <- withForeignPtr listPtr c_mcl_list_empty
  return $ rc /= 0

listInList :: MCLListH -> CIntPtr -> IO Bool
listInList (MCLListH listPtr) item = do
  withForeignPtr listPtr $ \ptr -> do
      rc <- c_mcl_list_in_list ptr item
      return $ rc > 0

listInListIf :: MCLListH -> MCLPred -> CIntPtr -> IO Bool
listInListIf (MCLListH listPtr) pred userData = do
  withForeignPtr listPtr $ \ptr -> do
    cb <- mk_mcl_pred pred
    rc <- c_mcl_list_in_list_if ptr cb userData
    freeHaskellFunPtr cb
    return $ rc > 0

listHead :: MCLListH -> IO (Maybe CIntPtr)
listHead (MCLListH listPtr) = do
  withForeignPtr listPtr $ \ptr -> do
    out <- malloc
    rc <- c_mcl_list_head ptr out
    v <- peek out
    free out
    if rc == 0
       then return $ Just $ fromIntegral v
       else return Nothing

listNthItem :: MCLListH -> Int -> IO (Maybe CIntPtr)
listNthItem (MCLListH listPtr) n = do
  withForeignPtr listPtr $ \ptr -> do
    out <- malloc
    rc <- c_mcl_list_nth_item ptr (fromIntegral n) out
    v <- peek out
    if rc == 0
       then return $ Just $ fromIntegral v
       else return Nothing

listReverse :: MCLListH -> IO ()
listReverse (MCLListH listPtr) = do
  withForeignPtr listPtr c_mcl_list_reverse

listSort :: MCLListH -> IO ()
listSort (MCLListH listPtr) = do
  withForeignPtr listPtr c_mcl_list_sort

listSortWith :: MCLListH -> MCLCmp -> CIntPtr -> IO ()
listSortWith (MCLListH listPtr) cmp userData = do
  withForeignPtr listPtr $ \ptr -> do
    cb <- mk_mcl_cmp cmp
    c_mcl_list_sort_with ptr cb userData
    freeHaskellFunPtr cb
