#include <mcl_rbtree.h>

#ifndef KERNEL
#include <stdlib.h>
#include <assert.h>
#endif

#ifdef KERNEL
#define malloc k_malloc
#define free   k_free
#define assert k_assert
#endif

typedef enum {
  MCL_RBTREE_NODE_RED
 ,MCL_RBTREE_NODE_BLACK
} MCLRBTreeNodeColor;

typedef struct MCLRBTreeNode {
  MCLItemType data;
  struct MCLRBTreeNode *left;
  struct MCLRBTreeNode *right;
  // The parent of root node is NULL.
  struct MCLRBTreeNode *parent;
  MCLRBTreeNodeColor    color;
} MCLRBTreeNode;

struct MCLRBTree {
  uint32_t       num_items;
  MCLRBTreeNode *root;
  // This function pointer is used to order
  // tree elements.
  MCLComparator  cmp;
  // This field is passed to the comparator.
  void          *user_data;
};

// The rotation functions return the new root of the rotated subtree.
MCLRBTreeNode *_mcl_rbtree_node_rotate_right(MCLRBTreeNode *node);
MCLRBTreeNode *_mcl_rbtree_node_rotate_left(MCLRBTreeNode *node);

// These rebalance functions return the parent of the root of the
// rebalanced subtree, and return the root of the rebalanced subtree
// into the variable new_subtree_root.
// The new_subtree_root is the root of the subtree that has been modified
// as part of the rebalancing.
MCLRBTreeNode *_mcl_rbtree_rebalance_1(
    MCLRBTreeNode *node, MCLRBTreeNode **new_subtree_root);
MCLRBTreeNode *_mcl_rbtree_rebalance_2(
    MCLRBTreeNode *node, MCLRBTreeNode **new_subtree_root);
MCLRBTreeNode *_mcl_rbtree_rebalance_3(
    MCLRBTreeNode *node, MCLRBTreeNode **new_subtree_root);
MCLRBTreeNode *_mcl_rbtree_rebalance_4(
    MCLRBTreeNode *node, MCLRBTreeNode **new_subtree_root);
MCLRBTreeNode *_mcl_rbtree_rebalance_5(
    MCLRBTreeNode *node, MCLRBTreeNode **new_subtree_root);

MCLRBTreeNode *_mcl_rbtree_delete_node_with_one_child(
    MCLRBTreeNode *node, MCLRBTreeNode **new_subtree_root);


MCLRBTreeNode *_mcl_rbtree_delete_rebalance_1(
    MCLRBTreeNode *node, MCLRBTreeNode **new_subtree_root);
MCLRBTreeNode *_mcl_rbtree_delete_rebalance_2(
    MCLRBTreeNode *node, MCLRBTreeNode **new_subtree_root);
MCLRBTreeNode *_mcl_rbtree_delete_rebalance_3(
    MCLRBTreeNode *node, MCLRBTreeNode **new_subtree_root);
MCLRBTreeNode *_mcl_rbtree_delete_rebalance_4(
    MCLRBTreeNode *node, MCLRBTreeNode **new_subtree_root);
MCLRBTreeNode *_mcl_rbtree_delete_rebalance_5(
    MCLRBTreeNode *node, MCLRBTreeNode **new_subtree_root);
MCLRBTreeNode *_mcl_rbtree_delete_rebalance_6(
    MCLRBTreeNode *node, MCLRBTreeNode **new_subtree_root);

// The node argument is Nullable. The color of a NULL
// node is BLACK by definition.
MCLRBTreeNodeColor _mcl_rbtree_node_color(MCLRBTreeNode *node)
{
  return (node) ? node->color : MCL_RBTREE_NODE_BLACK;
}

// The first argument to this function is Nullable.
// Passing NULL a the first argument results in a no-op.
void _mcl_rbtree_node_set_color(MCLRBTreeNode *node, MCLRBTreeNodeColor color)
{
  if (node)
  {
    node->color = color;
  }
}

// Return the parent of a node.
// The parent of a NULL node is
// NULL.
MCLRBTreeNode *_mcl_rbtree_node_parent(MCLRBTreeNode *node)
{
  return (node) ? node->parent : NULL;
}

// Return the grand parent of a node
// For nodes with no grand parents
// it returns NULL.
MCLRBTreeNode *_mcl_rbtree_node_grand_parent(MCLRBTreeNode *node)
{
  return _mcl_rbtree_node_parent(_mcl_rbtree_node_parent(node));
}

MCLRBTreeNode *_mcl_rbtree_node_sibling(MCLRBTreeNode *node)
{
  MCLRBTreeNode *parent = _mcl_rbtree_node_parent(node);

  if (parent && node == parent->left)
  {
    return parent->right;
  }
  else if (parent && node == parent->right)
  {
    return parent->left;
  }
  else
  {
    return NULL;
  }
}

MCLRBTreeNode *_mcl_rbtree_node_inorder_predecessor(MCLRBTreeNode *node)
{
  MCLRBTreeNode *curr = node->left;
  assert(curr);

  while (curr->right)
  {
    curr = curr->right;
  }

  return curr;
}

MCLRBTreeNode *_mcl_rbtree_node_inorder_successor(MCLRBTreeNode *node)
{
  MCLRBTreeNode *curr = node->right;
  assert(curr);

  while (curr->left)
  {
    curr = curr->left;
  }

  return curr;
}

// Return the uncle of a node.
// For nodes with no uncles
// it returns NULL.
MCLRBTreeNode *_mcl_rbtree_node_uncle(MCLRBTreeNode *node)
{
  MCLRBTreeNode *gparent = _mcl_rbtree_node_grand_parent(node);

  if (gparent)
  {
    MCLRBTreeNode *parent = _mcl_rbtree_node_parent(node);
    return (parent == gparent->left) ? (gparent->right) : (gparent->left);
  }
  else
  {
    return NULL;
  }
}

MCLRBTreeNode * _mcl_rbtree_node_rotate_right(MCLRBTreeNode *node)
{
  assert(node);

  MCLRBTreeNode *parent = node->parent;
  MCLRBTreeNode *left_subtree = node->left;
  MCLRBTreeNode *left_right_subtree = NULL;

  if (left_subtree)
  {
    left_right_subtree = left_subtree->right;
  }

  left_subtree->right = node;
  node->left = left_right_subtree;

  left_subtree->parent = parent;
  if (left_right_subtree)
  {
    left_right_subtree->parent = node;
  }
  node->parent = left_subtree;

  return left_subtree;
}

MCLRBTreeNode * _mcl_rbtree_node_rotate_left(MCLRBTreeNode *node)
{
  assert(node);

  MCLRBTreeNode *parent = node->parent;
  MCLRBTreeNode *right_subtree = node->right;
  MCLRBTreeNode *right_left_subtree = NULL;

  if (right_subtree)
  {
    right_left_subtree = right_subtree->left;
  }

  right_subtree->left = node;
  node->right = right_left_subtree;
  
  right_subtree->parent = parent;
  if (right_left_subtree)
  {
    right_left_subtree->parent = node;
  }
  node->parent = right_subtree;

  return right_subtree;
}

MCLRBTree *mcl_rbtree_create(MCLComparator cmp, void *user_data)
{
  MCLRBTree *tree = malloc(sizeof(MCLRBTree));

  tree->num_items = 0;
  tree->root      = NULL;
  tree->cmp       = cmp;
  tree->user_data = user_data;

  return tree;
}

// root is guranteed not to be NULL.
void _mcl_rbtree_bst_insert(MCLRBTreeNode **root
                           ,MCLRBTreeNode  *new_node
                           ,MCLComparator   cmp
                           ,void           *user_data
                           ,MCLRBTreeNode  *parent)
{
  assert (NULL != root);

  if (NULL == *root) {
    *root = new_node;
    new_node->parent = parent;
    return;
  }

  if (cmp(new_node->data, (*root)->data, user_data) < 0)
  {
    _mcl_rbtree_bst_insert(&(*root)->left, new_node, cmp, user_data, *root);
  }
  else
  {
    _mcl_rbtree_bst_insert(&(*root)->right, new_node, cmp, user_data, *root);
  }
}

void mcl_rbtree_insert(MCLRBTree *tree, MCLItemType item)
{
  MCLRBTreeNode *new_node = malloc(sizeof(MCLRBTreeNode));

  new_node->data   = item;
  new_node->parent = NULL;
  new_node->left   = NULL;
  new_node->right  = NULL;
  new_node->color  = MCL_RBTREE_NODE_RED;

  _mcl_rbtree_bst_insert(&tree->root
                        ,new_node
                        ,tree->cmp
                        ,tree->user_data
                        ,tree->root);

  MCLRBTreeNode *new_subtree_root = NULL;

  MCLRBTreeNode *up_one_node = _mcl_rbtree_rebalance_1(
      new_node, &new_subtree_root);

  assert(new_subtree_root);

  if (NULL == up_one_node)
  {
    tree->root = new_subtree_root;
  }

  tree->num_items++;
}

uint8_t mcl_rbtree_empty(MCLRBTree *tree)
{
  return (tree->num_items == 0);
}

MCLRBTreeNode *_mcl_rbtree_rebalance_1(
    MCLRBTreeNode *node, MCLRBTreeNode **new_subtree_root)
{
  assert(node);
  if (NULL == node->parent)
  {
    node->color = MCL_RBTREE_NODE_BLACK;
    *new_subtree_root = node;
    return node->parent;
  }
  else
  {
    return _mcl_rbtree_rebalance_2(node, new_subtree_root);
  }
}

MCLRBTreeNode *_mcl_rbtree_rebalance_2(
    MCLRBTreeNode *node, MCLRBTreeNode **new_subtree_root)
{
  assert(node);
  if (_mcl_rbtree_node_color(
        _mcl_rbtree_node_parent(node)) == MCL_RBTREE_NODE_BLACK)
  {
    *new_subtree_root = node;
    return node->parent;
  }
  else
  {
    return _mcl_rbtree_rebalance_3(node, new_subtree_root);
  }
}

MCLRBTreeNode *_mcl_rbtree_rebalance_3(
    MCLRBTreeNode *node, MCLRBTreeNode **new_subtree_root)
{
  MCLRBTreeNode *uncle = _mcl_rbtree_node_uncle(node);
  MCLRBTreeNode *gparent = _mcl_rbtree_node_grand_parent(node);

  assert(gparent);

  if ((NULL != uncle) && (MCL_RBTREE_NODE_RED == uncle->color))
  {
    node->parent->color = MCL_RBTREE_NODE_BLACK;
    uncle->color = MCL_RBTREE_NODE_BLACK;

    gparent->color = MCL_RBTREE_NODE_RED;

    return _mcl_rbtree_rebalance_1(gparent, new_subtree_root);
  }
  else
  {
    return _mcl_rbtree_rebalance_4(node, new_subtree_root);
  }
}

MCLRBTreeNode *_mcl_rbtree_rebalance_4(
    MCLRBTreeNode *node, MCLRBTreeNode **new_subtree_root)
{
  MCLRBTreeNode *gparent = _mcl_rbtree_node_grand_parent(node);
  MCLRBTreeNode *parent = _mcl_rbtree_node_parent(node);
  
  assert(gparent);
  assert(parent);

  if ((node == parent->right) && (parent == gparent->left))
  {
    parent->right = _mcl_rbtree_node_rotate_left(parent);
    node = node->left;
  }
  else if ((node == parent->left) && (parent == gparent->right))
  {
    parent->left = _mcl_rbtree_node_rotate_right(parent);
    node = node->right;
  }

  return _mcl_rbtree_rebalance_5(node, new_subtree_root);
}

MCLRBTreeNode *_mcl_rbtree_rebalance_5(
    MCLRBTreeNode *node, MCLRBTreeNode **new_subtree_root)
{
  MCLRBTreeNode *gparent = _mcl_rbtree_node_grand_parent(node);
  MCLRBTreeNode *parent  = _mcl_rbtree_node_parent(node);

  assert(gparent);
  assert(parent);

  parent->color = MCL_RBTREE_NODE_BLACK;
  gparent->color = MCL_RBTREE_NODE_RED;

  MCLRBTreeNode *parent_gparent = _mcl_rbtree_node_parent(gparent);
  int gparent_direction = 0;

  if (parent_gparent && parent_gparent->left == gparent)
  {
    gparent_direction = -1;
  }
  else if (parent_gparent && parent_gparent->right == gparent)
  {
    gparent_direction = 1;
  }

  MCLRBTreeNode *new_root = NULL;
  if (node == parent->left)
  {
    new_root = _mcl_rbtree_node_rotate_right(gparent);
  }
  else
  {
    new_root = _mcl_rbtree_node_rotate_left(gparent);
  }

  assert(new_root);

  if (gparent_direction < 0)
  {
    parent_gparent->left = new_root;
  }
  else if (gparent_direction > 0)
  {
    parent_gparent->right = new_root;
  }

  *new_subtree_root = new_root;
  return parent_gparent;
}

MCLRBTreeNode *_mcl_rbtree_find_node(MCLRBTreeNode *root
                                    ,MCLItemType item
                                    ,MCLComparator cmp
                                    ,void *user_data)
{
  if (root->data == item || !root)
  {
    return root;
  }

  if (cmp(item, root->data, user_data) < 0)
  {
    return _mcl_rbtree_find_node(root->left, item, cmp, user_data);
  }
  else
  {
    return _mcl_rbtree_find_node(root->right, item, cmp, user_data);
  }
}

uint8_t mcl_rbtree_delete(MCLRBTree *tree, MCLItemType item)
{
  MCLRBTreeNode *node_to_delete
    = _mcl_rbtree_find_node(tree->root, item, tree->cmp, tree->user_data);

  if (node_to_delete)
  {
    if (node_to_delete->left && node_to_delete->right)
    {
      // Replace inorder successor with node_to_delete
      MCLRBTreeNode *succ  = _mcl_rbtree_node_inorder_successor(node_to_delete);
      MCLItemType    temp  = succ->data;
      succ->data           = node_to_delete->data;
      node_to_delete->data = temp;

      node_to_delete       = succ;
    }

    MCLRBTreeNode *new_subtree_root = NULL;

    MCLRBTreeNode *up_one_node
      = _mcl_rbtree_delete_node_with_one_child(
          node_to_delete, &new_subtree_root);

    if (NULL == up_one_node)
    {
      tree->root = new_subtree_root;
    }

    return 0;
  }
  else
  {
    return 1;
  }
}

MCLRBTreeNode *_mcl_rbtree_delete_node_with_one_child(
    MCLRBTreeNode *node, MCLRBTreeNode **new_subtree_root)
{
  // Precondition, node has at most one
  // non-NULL children.
  assert(!(node->left && node->right));
}
