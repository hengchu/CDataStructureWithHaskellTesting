#include <stdio.h>
#include <assert.h>
#include <mcl_rbtree.h>

int main(int argc, char *argv[])
{
  int data[] = {9, 8, 7, 6, 5, 4, 3, 2, 1, 2, 3, 4, 5, 6, 7, 8, 9};

  MCLRBTree *tree = mcl_rbtree_create(mcl_default_comparator, NULL);

  assert(mcl_rbtree_empty(tree));

  int num_items = sizeof(data) / sizeof(*data);

  for (int i = 0; i < num_items; i++)
  {
    mcl_rbtree_insert(tree, data[i]);
    assert(!mcl_rbtree_empty(tree));
  }
}
