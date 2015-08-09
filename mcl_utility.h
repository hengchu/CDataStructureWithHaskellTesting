#ifndef INCLUDED_MCL_UTILITY
#define INCLUDED_MCL_UTILITY

#include <stdint.h>

typedef intptr_t       MCLItemType;
typedef struct MCLList MCLList;

// A predicate that should return non-zero value
// when item matches, and zero otherwise.
// The void * argument is a user data that would
// be passed into the predicate when it is run.
typedef int (*MCLPredicate) (MCLItemType, void *);

// A comparator used to sort the list with.
// Item A would be placed in front of item
// B if cmp(A, B) < 0.
// The third argument to the comparator is
// user data.
typedef int (*MCLComparator) (MCLItemType, MCLItemType, void *);

// A visitor used to traverse the data structures.
typedef void (*MCLVisitor) (MCLItemType, void *);

// This default comparator simply compares the two elements
// with operator '<'. It ignores the user_data argument.
int mcl_default_comparator(MCLItemType left
                          ,MCLItemType right
                          ,void *user_data);

#endif
