#ifndef INCLUDED_MCL_RBTREE
#define INCLUDED_MCL_RBTREE

#ifndef INCLUDED_MCL_UTILITY
#include <mcl_utility.h>
#endif

typedef struct MCLRBTree MCLRBTree;

// Create an empty red black tree, the ordering
// of the elements in this tree is determined
// by this comparator.
// The user_data argument is passed to the comparator
// every time it is called.
//
// Note that you can use mcl_default_comparator, which
// is the '<' operator if you don't want to use a special
// comparator.
MCLRBTree *mcl_rbtree_create(MCLComparator cmp, void *user_data);

// Destroy this tree.
// Note that it does not free any
// element stored in this tree.
void mcl_rbtree_destroy(MCLRBTree *tree);

// Insert an item into the red black tree.
void mcl_rbtree_insert(MCLRBTree *tree, MCLItemType item);

// Delete an item from the red black tree.
// Returns zero on success
// Returns non-zero value on failure.
uint8_t mcl_rbtree_delete(MCLRBTree *tree, MCLItemType item);

// Return non-zero value if tree is empty
// Return zero if tree is non-empty
uint8_t mcl_rbtree_empty(MCLRBTree *tree);

// Return the number of items in this tree.
uint32_t mcl_rbtree_num_items(MCLRBTree *tree);

// Return the depth of this tree.
// Note that the depth of an empty tree
// is defined to be 0.
uint32_t mcl_rbtree_depth(MCLRBTree *tree);

#endif
