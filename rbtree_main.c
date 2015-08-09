#include <stdio.h>
#include <assert.h>
#include <mcl_rbtree.h>

void visitor(MCLItemType item, void *user_data)
{
  printf("%lu\n", item);
}

int main(int argc, char *argv[])
{
  int data[] = { 265,30,121,181,388,425,435,256,461,2,3,118 };

  MCLRBTree *tree = mcl_rbtree_create(mcl_default_comparator, NULL);

  assert(mcl_rbtree_empty(tree));

  int num_items = sizeof(data) / sizeof(*data);

  for (int i = 0; i < num_items; i++)
  {
    mcl_rbtree_insert(tree, data[i]);
    assert(!mcl_rbtree_empty(tree));
  }

  printf("tree depth: %d\n", mcl_rbtree_depth(tree));

  mcl_rbtree_visit_pre_order(tree, visitor, 0);

  //for (int i = 0; i < num_items; i++)
  //{
  //  mcl_rbtree_delete(tree, data[i]);
  //}

  mcl_rbtree_destroy(tree);

  return 0;
}
