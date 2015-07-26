{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module MCLList
( newList
, listNumItems
, listAppendFront
, listRemove
, listIsEmpty
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

listIsEmpty :: MCLListH -> IO Int
listIsEmpty (MCLListH listPtr) =
  withForeignPtr listPtr c_mcl_list_empty
