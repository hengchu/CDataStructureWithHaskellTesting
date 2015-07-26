#ifndef INCLUDED_MCL_LIST
#define INCLUDED_MCL_LIST

#include <stdint.h>

typedef intptr_t       MCLItemType;
typedef struct MCLList MCLList;

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

#endif
