#ifndef INCLUDED_MCL_LIST
#define INCLUDED_MCL_LIST

#ifndef INCLUDED_MCL_UTILITY
#include <mcl_utility.h>
#endif

// Appends item to the front of the list.
void mcl_list_append_front(MCLList *list, MCLItemType item);

// Remove the item from the list.
// Return non-zero value on failure.
// Return 0 on success.
// Note that it does not free the item itself
// if it needs any destructor called.
int mcl_list_remove(MCLList *list, MCLItemType item_to_remove);

// Return an empty list.
MCLList *mcl_list_create();

// Destroy this list.
// Note that it does not free any
// element stored in the list.
void mcl_list_destroy(MCLList *list);

// Return the number of items currently
// in the list.
uint32_t mcl_list_num_items(MCLList *list);

// Return non-zero value if list is empty.
// Return zero if list is not empty.
uint8_t mcl_list_empty(MCLList *list);

// Returns non-zero value if this item is in the list
// Return zero if item is not in the list
uint8_t mcl_list_in_list(MCLList *list, MCLItemType item);

// This is a generalization of mcl_list_in_list
// It returns non-zero value if there exists at
// least one value that matches the predicate.
// Otherwise it returns zero.
uint8_t mcl_list_in_list_if(MCLList *list
                           ,MCLPredicate pred
                           ,void *user_data);

// Return the head of the list into
// out_item if the list is not empty.
// Return zero on success
// Return non-zero value on failure (list is empty)
uint8_t mcl_list_head(MCLList *list, MCLItemType *out_item);

// Write the nth (zero-based) item into out_item.
// Return non-zero value on failure.
// Return 0 on success.
uint8_t mcl_list_nth_item(MCLList *list, uint32_t n, MCLItemType *out_item);

// Reverse this list.
void mcl_list_reverse(MCLList *list);

// Sort this list.
void mcl_list_sort(MCLList *list);

// Sort this list with a comparator
void mcl_list_sort_with(MCLList *list, MCLComparator cmp, void *user_data);

#endif
