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

  MCLRBTreeNode *sentinel;

  // This function pointer is used to order
  // tree elements.
  MCLComparator  cmp;
  //
  // This field is passed to the comparator.
  void          *user_data;
};

int _mcl_rbtree_validate_pointers(
     MCLRBTree *tree
    ,MCLRBTreeNode *node
    ,MCLRBTreeNode *parent)
{
  if (node == tree->sentinel)
  {
    return 1;
  }
  else
  {
    return (node->parent == parent) 
      && _mcl_rbtree_validate_pointers(tree, node->left, node)
      && _mcl_rbtree_validate_pointers(tree, node->right, node);
  }
}

MCLRBTreeNode *_mcl_rbtree_node_find(MCLRBTree *tree, MCLItemType item)
{
  MCLRBTreeNode *x = tree->root;

  while (x != tree->sentinel && x->data != item)
  {

    int cmp_result = tree->cmp(item, x->data, tree->user_data);

    if (cmp_result < 0)
    {
      x = x->left;
    }
    else
    {
      x = x->right;
    }
  }

  return x;
}

MCLRBTreeNode *_mcl_rbtree_minimum(MCLRBTree *tree, MCLRBTreeNode *node)
{
  while (node != tree->sentinel)
  {
    node = node->left;
  }
  return node;
}

void _mcl_rbtree_left_rotate(MCLRBTree *tree, MCLRBTreeNode *node)
{
  MCLRBTreeNode *right_child = node->right;
  node->right = right_child->left;

  if (right_child->left != tree->sentinel)
  {
    right_child->left->parent = node;
  }

  if (node->parent == tree->sentinel)
  {
    tree->root = right_child;
    right_child->parent = tree->sentinel;
  }
  else if (node == node->parent->left)
  {
    node->parent->left = right_child;
  }
  else
  {
    node->parent->right = right_child;
  }

  right_child->left = node;
  right_child->parent = node->parent;
  node->parent = right_child;

  assert(_mcl_rbtree_validate_pointers(tree, tree->root, tree->sentinel));
}

void _mcl_rbtree_right_rotate(MCLRBTree *tree, MCLRBTreeNode *node)
{
  MCLRBTreeNode *left_child = node->left;
  node->left = left_child->right;

  if (left_child->right != tree->sentinel)
  {
    left_child->right->parent = node;
  }
  left_child->parent = node->parent;
  if (node->parent == tree->sentinel)
  {
    tree->root = left_child;
    left_child->parent = tree->sentinel;
  }
  else if (node == node->parent->right)
  {
    node->parent->right = left_child;
  }
  else
  {
    node->parent->left = left_child;
  }
  left_child->right = node;
  left_child->parent = node->parent;
  node->parent = left_child;

  assert(_mcl_rbtree_validate_pointers(tree, tree->root, tree->sentinel));
}

void _mcl_rbtree_transplant(MCLRBTree *tree, MCLRBTreeNode *u, MCLRBTreeNode *v)
{
  if (u->parent == tree->sentinel)
  {
    tree->root = v;
  }
  else if (u == u->parent->left)
  {
    u->parent->left = v;
  }
  else
  {
    u->parent->right = v;
  }
  v->parent = u->parent;
}

MCLRBTree *mcl_rbtree_create(MCLComparator cmp, void *user_data)
{
  MCLRBTree *tree = malloc(sizeof(MCLRBTree));

  tree->num_items = 0;
  tree->cmp       = cmp;
  tree->user_data = user_data;

  tree->sentinel  = malloc(sizeof(MCLRBTreeNode));

  tree->sentinel->left   = tree->sentinel;
  tree->sentinel->right  = tree->sentinel;
  tree->sentinel->parent = tree->sentinel;
  tree->sentinel->color  = MCL_RBTREE_NODE_BLACK;

  tree->root = tree->sentinel;

  return tree;
}

void _mcl_rbtree_insert_fixup(MCLRBTree *tree, MCLRBTreeNode *new_node)
{

  MCLRBTreeNode *z = new_node;

  while (z->parent->color == MCL_RBTREE_NODE_RED)
  {
    if (z->parent == z->parent->parent->left)
    {
      MCLRBTreeNode *y = z->parent->parent->right;
      if (y->color == MCL_RBTREE_NODE_RED)
      {
        z->parent->color = MCL_RBTREE_NODE_BLACK;
        y->color = MCL_RBTREE_NODE_BLACK;
        z->parent->parent->color = MCL_RBTREE_NODE_RED;
        z = z->parent->parent;
      }
      else
      {
        if (z == z->parent->right)
        {
          z = z->parent;
          _mcl_rbtree_left_rotate(tree, z);
        }
        z->parent->color = MCL_RBTREE_NODE_BLACK;
        z->parent->parent->color = MCL_RBTREE_NODE_RED;
        _mcl_rbtree_right_rotate(tree, z->parent->parent);
      }
    }
    else
    {
      MCLRBTreeNode *y = z->parent->parent->left;
      if (y->color == MCL_RBTREE_NODE_RED)
      {
        z->parent->color = MCL_RBTREE_NODE_BLACK;
        y->color = MCL_RBTREE_NODE_BLACK;
        z->parent->parent->color = MCL_RBTREE_NODE_RED;
        z = z->parent->parent;
      }
      else
      {
        if (z == z->parent->left)
        {
          z = z->parent;
          _mcl_rbtree_right_rotate(tree, z);
        }
        z->parent->color = MCL_RBTREE_NODE_BLACK;
        z->parent->parent->color = MCL_RBTREE_NODE_RED;
        _mcl_rbtree_left_rotate(tree, z->parent->parent);
      }
    }

    tree->sentinel->color = MCL_RBTREE_NODE_BLACK;
  }

  tree->root->color = MCL_RBTREE_NODE_BLACK;

  assert(_mcl_rbtree_validate_pointers(tree, tree->root, tree->sentinel));
}

void _mcl_rbtree_insert(MCLRBTree *tree, MCLRBTreeNode *new_node)
{
  MCLRBTreeNode *z = new_node;

  MCLRBTreeNode *y = tree->sentinel;
  MCLRBTreeNode *x = tree->root;

  while (x != tree->sentinel)
  {
    y = x;
    if (tree->cmp(z->data, x->data, tree->user_data) < 0)
    {
      x = x->left;
    }
    else
    {
      x = x->right;
    }
  }
  z->parent = y;
  if (y == tree->sentinel)
  {
    tree->root = z;
  }
  else if (tree->cmp(z->data, x->data, tree->user_data) < 0)
  {
    y->left = z;
  }
  else
  {
    y->right = z;
  }

  z->left = tree->sentinel;
  z->right = tree->sentinel;
  z->color = MCL_RBTREE_NODE_RED;

  _mcl_rbtree_insert_fixup(tree, z);

  assert(_mcl_rbtree_validate_pointers(tree, tree->root, tree->sentinel));
}

void mcl_rbtree_insert(MCLRBTree *tree, MCLItemType item)
{
  MCLRBTreeNode *new_node = malloc(sizeof(MCLRBTreeNode));
  new_node->data = item;

  _mcl_rbtree_insert(tree, new_node);

  tree->num_items++;

  assert(_mcl_rbtree_validate_pointers(tree, tree->root, tree->sentinel));
}

void _mcl_rbtree_delete_fixup(MCLRBTree *tree, MCLRBTreeNode *node)
{
  MCLRBTreeNode *x = node;

  while (x != tree->root && x->color == MCL_RBTREE_NODE_BLACK)
  {
    if (x == x->parent->left)
    {
      MCLRBTreeNode *w = x->parent->right;
      if (w->color == MCL_RBTREE_NODE_RED)
      {
        w->color = MCL_RBTREE_NODE_BLACK;
        x->parent->color = MCL_RBTREE_NODE_RED;
        _mcl_rbtree_left_rotate(tree, x->parent);
        w = x->parent->right;
      }
      if (w->left->color == MCL_RBTREE_NODE_BLACK 
          && w->right->color == MCL_RBTREE_NODE_BLACK)
      {
        w->color = MCL_RBTREE_NODE_RED;
        x = x->parent;
      }
      else if (w->right->color == MCL_RBTREE_NODE_BLACK)
      {
        w->left->color = MCL_RBTREE_NODE_BLACK;
        w->color = MCL_RBTREE_NODE_RED;
        _mcl_rbtree_right_rotate(tree, w);
        w = x->parent->right;
      }

      w->color = x->parent->color;
      x->parent->color = MCL_RBTREE_NODE_BLACK;
      w->right->color = MCL_RBTREE_NODE_BLACK;
      _mcl_rbtree_left_rotate(tree, x->parent);
      x = tree->root;
    }
    else
    {
      MCLRBTreeNode *w = x->parent->left;
      if (w->color == MCL_RBTREE_NODE_RED)
      {
        w->color = MCL_RBTREE_NODE_BLACK;
        x->parent->color = MCL_RBTREE_NODE_RED;
        _mcl_rbtree_right_rotate(tree, x->parent);
        w = x->parent->left;
      }
      if (w->right->color == MCL_RBTREE_NODE_BLACK 
          && w->left->color == MCL_RBTREE_NODE_BLACK)
      {
        w->color = MCL_RBTREE_NODE_RED;
        x = x->parent;
      }
      else if (w->left->color == MCL_RBTREE_NODE_BLACK)
      {
        w->right->color = MCL_RBTREE_NODE_BLACK;
        w->color = MCL_RBTREE_NODE_RED;
        _mcl_rbtree_left_rotate(tree, w);
        w = x->parent->left;
      }

      w->color = x->parent->color;
      x->parent->color = MCL_RBTREE_NODE_BLACK;
      w->left->color = MCL_RBTREE_NODE_BLACK;
      _mcl_rbtree_right_rotate(tree, x->parent);
      x = tree->root;
    }
  }

  x->color = MCL_RBTREE_NODE_BLACK;
}

void _mcl_rbtree_delete(MCLRBTree *tree, MCLRBTreeNode *node)
{
  MCLRBTreeNode *z = node;

  MCLRBTreeNode *y = z;
  MCLRBTreeNode *x = NULL;
  MCLRBTreeNodeColor y_orig_color = y->color;

  if (z->left == tree->sentinel)
  {
    x = z->right;
    _mcl_rbtree_transplant(tree, z, z->right);
  }
  else if (z->right == tree->sentinel)
  {
    x = z->left;
    _mcl_rbtree_transplant(tree, z, z->left);
  }
  else
  {
    y = _mcl_rbtree_minimum(tree, z->right);
    x = y->right;

    if (y->parent == z)
    {
      x->parent = y;
    }
    else
    {
      _mcl_rbtree_transplant(tree, y, y->right);
      y->right = z->right;
      y->right->parent = y;
    }

    _mcl_rbtree_transplant(tree, z, y);
    y->left = z->left;
    y->left->parent = y;
    y->color = z->color;
  }

  if (y_orig_color == MCL_RBTREE_NODE_BLACK)
  {
    _mcl_rbtree_delete_fixup(tree, x);
  }
}

uint8_t mcl_rbtree_delete(MCLRBTree *tree, MCLItemType item)
{
  MCLRBTreeNode *node_to_delete = _mcl_rbtree_node_find(tree, item);  
  tree->num_items--;

  if (node_to_delete == tree->sentinel)
  {
    return 1;
  }
  else
  {
    _mcl_rbtree_delete(tree, node_to_delete);
    free(node_to_delete);
    assert(_mcl_rbtree_validate_pointers(tree, tree->root, tree->sentinel));
    return 0;
  }
}

void _mcl_rbtree_delete_root(MCLRBTree *tree)
{
  MCLItemType item = tree->root->data;

  mcl_rbtree_delete(tree, item);
}

void mcl_rbtree_destroy(MCLRBTree *tree)
{
  while (tree->root != tree->sentinel)
  {
    _mcl_rbtree_delete_root(tree);
  }

  free(tree->sentinel);
  free(tree);
}

uint8_t mcl_rbtree_empty(MCLRBTree *tree)
{
  return (tree->num_items == 0);
}
