#include <mcl_rbtree.h>

#ifndef KERNEL
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#endif

#ifdef KERNEL
#define malloc  k_malloc
#define realloc k_realloc
#define free    k_free
#define assert  k_assert
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

int _mcl_rbtree_verify_property1_helper(MCLRBTree *tree, MCLRBTreeNode *node)
{
  if (node == tree->sentinel)
  {
    return node->color == MCL_RBTREE_NODE_BLACK;
  }

  int rc = node->color == MCL_RBTREE_NODE_BLACK ||
           node->color == MCL_RBTREE_NODE_RED;

  return rc && _mcl_rbtree_verify_property1_helper(tree, node->left) &&
               _mcl_rbtree_verify_property1_helper(tree, node->right);
}

int _mcl_rbtree_verify_property1(MCLRBTree *tree)
{
  return _mcl_rbtree_verify_property1_helper(tree, tree->root);
}

int _mcl_rbtree_verify_property2(MCLRBTree *tree)
{
  return tree->root->color == MCL_RBTREE_NODE_BLACK;
}

int _mcl_rbtree_verify_property3(MCLRBTree *tree)
{
  return tree->sentinel->color == MCL_RBTREE_NODE_BLACK;
}

int _mcl_rbtree_verify_property4_helper(MCLRBTree *tree, MCLRBTreeNode *node)
{
  if (node->color == MCL_RBTREE_NODE_RED)
  {
    int rc = node->left->color == MCL_RBTREE_NODE_BLACK &&
             node->right->color == MCL_RBTREE_NODE_BLACK;

    return rc && _mcl_rbtree_verify_property4_helper(tree, node->left) &&
                 _mcl_rbtree_verify_property4_helper(tree, node->right);
  }
  else if (node != tree->sentinel)
  {
    return _mcl_rbtree_verify_property4_helper(tree, node->left) &&
           _mcl_rbtree_verify_property4_helper(tree, node->right);
  }
  else
  {
    return 1;
  }
}

int _mcl_rbtree_verify_property4(MCLRBTree *tree)
{
  return _mcl_rbtree_verify_property4_helper(tree, tree->root);
}

// returns the number of black nodes of all paths starting from this node
// to nil descendants.
void _mcl_rbtree_verify_property5_helper(MCLRBTree *tree
                                        ,MCLRBTreeNode *node
                                        ,uint32_t *num_paths
                                        ,uint32_t **lengths
                                        ,uint32_t current_length)
{
  // Base case
  if (node == tree->sentinel)
  {
    *num_paths          += 1;
    *lengths             = realloc(*lengths, *num_paths * sizeof(uint32_t));
    *lengths[*num_paths-1] = current_length + 1;
    return;
  }

  if (node->color == MCL_RBTREE_NODE_BLACK)
  {
    _mcl_rbtree_verify_property5_helper(
        tree, node->left, num_paths, lengths, current_length+1);
  }
  else
  {
    _mcl_rbtree_verify_property5_helper(
        tree, node->right, num_paths, lengths, current_length);
  }
}

int _mcl_rbtree_verify_property5(MCLRBTree *tree)
{
  uint32_t num_paths = 0;
  uint32_t *lengths  = NULL;

  _mcl_rbtree_verify_property5_helper(
      tree, tree->root, &num_paths, &lengths, 0);

  int retval = 1;
  for (uint32_t i = 0; i < num_paths; i++)
  {
    if (lengths[i] != lengths[0])
    {
      retval = 1;
      break;
    }
  }

  free(lengths);
  return retval;
}

int _mcl_rbtree_verify(MCLRBTree *tree)
{
  return _mcl_rbtree_verify_property1(tree) &&
         _mcl_rbtree_verify_property2(tree) &&
         _mcl_rbtree_verify_property3(tree) &&
         _mcl_rbtree_verify_property4(tree) &&
         _mcl_rbtree_verify_property5(tree);
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

uint8_t mcl_rbtree_find(MCLRBTree *tree, MCLItemType item)
{
  MCLRBTreeNode *node = _mcl_rbtree_node_find(tree, item);

  if (node == tree->sentinel)
  {
    return 0;
  }
  else
  {
    return 1;
  }
}

uint8_t _mcl_rbtree_find_if_helper(MCLRBTree *tree
                                  ,MCLRBTreeNode *node
                                  ,MCLPredicate pred
                                  ,void *user_data)
{
  if (node == tree->sentinel)
  {
    return 0;
  }

  if (pred(node->data, user_data))
  {
    return 1;
  }

  return _mcl_rbtree_find_if_helper(tree, node->left, pred, user_data)
      || _mcl_rbtree_find_if_helper(tree, node->right, pred, user_data);
}

uint8_t mcl_rbtree_find_if(MCLRBTree *tree, MCLPredicate pred, void *user_data)
{
  return _mcl_rbtree_find_if_helper(tree, tree->root, pred, user_data);
}

MCLRBTreeNode *_mcl_rbtree_minimum(MCLRBTree *tree, MCLRBTreeNode *node)
{
  while (node->left != tree->sentinel)
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

#ifdef DEBUG
  assert(_mcl_rbtree_validate_pointers(tree, tree->root, tree->sentinel));
#endif
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

#ifdef DEBUG
  assert(_mcl_rbtree_validate_pointers(tree, tree->root, tree->sentinel));
#endif
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

MCLRBTree *mcl_rbtree_create_default()
{
  return mcl_rbtree_create(mcl_default_comparator, NULL);
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

#ifdef DEBUG
  assert(_mcl_rbtree_validate_pointers(tree, tree->root, tree->sentinel));
#endif
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
  else if (tree->cmp(z->data, y->data, tree->user_data) < 0)
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

#ifdef DEBUG
  assert(_mcl_rbtree_validate_pointers(tree, tree->root, tree->sentinel));
#endif
}

void mcl_rbtree_insert(MCLRBTree *tree, MCLItemType item)
{
  MCLRBTreeNode *new_node = malloc(sizeof(MCLRBTreeNode));
  new_node->data = item;

  _mcl_rbtree_insert(tree, new_node);

  tree->num_items++;

#ifdef DEBUG
  assert(_mcl_rbtree_validate_pointers(tree, tree->root, tree->sentinel));
  assert(_mcl_rbtree_verify(tree));
#endif
}

void _mcl_rbtree_delete_case1(MCLRBTree *tree, MCLRBTreeNode *node);
void _mcl_rbtree_delete_case2(MCLRBTree *tree, MCLRBTreeNode *node);
void _mcl_rbtree_delete_case3(MCLRBTree *tree, MCLRBTreeNode *node);
void _mcl_rbtree_delete_case4(MCLRBTree *tree, MCLRBTreeNode *node);
void _mcl_rbtree_delete_case5(MCLRBTree *tree, MCLRBTreeNode *node);
void _mcl_rbtree_delete_case6(MCLRBTree *tree, MCLRBTreeNode *node);

MCLRBTreeNode *_mcl_rbtree_sibling(MCLRBTree *tree, MCLRBTreeNode *node)
{
  if (node == node->parent->left)
  {
    return node->parent->right;
  }
  else
  {
    return node->parent->left;
  }
}

//reference: https://en.wikipedia.org/wiki/Red%E2%80%93black_tree#Removal
void _mcl_rbtree_delete_fixup(MCLRBTree *tree, MCLRBTreeNode *node)
{
  _mcl_rbtree_delete_case1(tree, node);

  tree->root->color = MCL_RBTREE_NODE_BLACK;

  tree->sentinel->color = MCL_RBTREE_NODE_BLACK;
  tree->sentinel->parent = tree->sentinel;
  tree->sentinel->left = tree->sentinel;
  tree->sentinel->right = tree->sentinel;

}

void _mcl_rbtree_delete_case1(MCLRBTree *tree, MCLRBTreeNode *node)
{
  tree->sentinel->color = MCL_RBTREE_NODE_BLACK;
  if (node != tree->root)
  {
    _mcl_rbtree_delete_case2(tree, node);
  }
}

void _mcl_rbtree_delete_case2(MCLRBTree *tree, MCLRBTreeNode *node)
{
  MCLRBTreeNode *sibling = _mcl_rbtree_sibling(tree, node);

  if (sibling->color == MCL_RBTREE_NODE_RED)
  {
    node->parent->color = MCL_RBTREE_NODE_RED;
    sibling->color = MCL_RBTREE_NODE_BLACK;
    if (node == node->parent->left)
    {
      _mcl_rbtree_left_rotate(tree, node->parent);
    }
    else
    {
      _mcl_rbtree_right_rotate(tree, node->parent);
    }
  }

  _mcl_rbtree_delete_case3(tree, node);
}

void _mcl_rbtree_delete_case3(MCLRBTree *tree, MCLRBTreeNode *node)
{
  MCLRBTreeNode *sibling = _mcl_rbtree_sibling(tree, node);

  if (node->parent->color == MCL_RBTREE_NODE_BLACK &&
      sibling->color == MCL_RBTREE_NODE_BLACK &&
      sibling->left->color == MCL_RBTREE_NODE_BLACK &&
      sibling->right->color == MCL_RBTREE_NODE_BLACK)
  {
    sibling->color = MCL_RBTREE_NODE_RED;
    _mcl_rbtree_delete_case1(tree, node->parent);
  }
  else
  {
    _mcl_rbtree_delete_case4(tree, node);
  }
}

void _mcl_rbtree_delete_case4(MCLRBTree *tree, MCLRBTreeNode *node)
{
  MCLRBTreeNode *sibling = _mcl_rbtree_sibling(tree, node);

  if (node->parent->color == MCL_RBTREE_NODE_RED &&
      sibling->color == MCL_RBTREE_NODE_BLACK &&
      sibling->left->color == MCL_RBTREE_NODE_BLACK &&
      sibling->right->color == MCL_RBTREE_NODE_BLACK)
  {
    sibling->color = MCL_RBTREE_NODE_RED;
    node->parent->color = MCL_RBTREE_NODE_BLACK;
  }
  else
  {
    _mcl_rbtree_delete_case5(tree, node);
  }
}

void _mcl_rbtree_delete_case5(MCLRBTree *tree, MCLRBTreeNode *node)
{
  MCLRBTreeNode *sibling = _mcl_rbtree_sibling(tree, node);

  if (sibling->color == MCL_RBTREE_NODE_BLACK)
  {
    if (node == node->parent->left &&
        sibling->right->color == MCL_RBTREE_NODE_BLACK &&
        sibling->left->color == MCL_RBTREE_NODE_RED)
    {
      sibling->color = MCL_RBTREE_NODE_RED;
      sibling->left->color = MCL_RBTREE_NODE_BLACK;
      _mcl_rbtree_right_rotate(tree, sibling);
    }
    else if (node == node->parent->right &&
             sibling->left->color == MCL_RBTREE_NODE_BLACK &&
             sibling->right->color == MCL_RBTREE_NODE_RED)
    {
      sibling->color = MCL_RBTREE_NODE_RED;
      sibling->right->color = MCL_RBTREE_NODE_BLACK;
      _mcl_rbtree_left_rotate(tree, sibling);
    }
  }

  _mcl_rbtree_delete_case6(tree, node);
}

void _mcl_rbtree_delete_case6(MCLRBTree *tree, MCLRBTreeNode *node)
{
  MCLRBTreeNode *sibling = _mcl_rbtree_sibling(tree, node);

  sibling->color = node->parent->color;
  node->parent->color = MCL_RBTREE_NODE_BLACK;

  if (node == node->parent->left)
  {
    sibling->right->color = MCL_RBTREE_NODE_BLACK;
    _mcl_rbtree_left_rotate(tree, node->parent);
  }
  else
  {
    sibling->left->color = MCL_RBTREE_NODE_BLACK;
    _mcl_rbtree_right_rotate(tree, node->parent);
  }

}

void _mcl_rbtree_delete_node_with_one_child(MCLRBTree *tree
                                           ,MCLRBTreeNode *node)
{
  MCLRBTreeNode *child = tree->sentinel;

  if (node->left == tree->sentinel)
  {
    child = node->right;
  }
  else if (node->right == tree->sentinel)
  {
    child = node->left;
  }

  if (node == tree->root)
  {
    tree->root = child;
  }
  else if (node->parent->left == node)
  {
    node->parent->left = child;
  }
  else
  {
    node->parent->right = child;
  }

  child->parent = node->parent;

  if (node->color == MCL_RBTREE_NODE_BLACK)
  {
    if (child->color == MCL_RBTREE_NODE_RED)
    {
      child->color = MCL_RBTREE_NODE_BLACK;
    }
    else
    {
      _mcl_rbtree_delete_fixup(tree, child);
    }
  }
}

void _mcl_rbtree_delete(MCLRBTree *tree, MCLRBTreeNode *node)
{
  if (node->left != tree->sentinel &&
      node->right != tree->sentinel)
  {
    MCLRBTreeNode *successor = _mcl_rbtree_minimum(tree, node->right);
    MCLItemType temp = node->data;
    node->data = successor->data;
    successor->data = temp;

    node = successor;
  }

  _mcl_rbtree_delete_node_with_one_child(tree, node);

  free(node);
}

uint8_t mcl_rbtree_delete(MCLRBTree *tree, MCLItemType item)
{
  MCLRBTreeNode *node_to_delete = _mcl_rbtree_node_find(tree, item);  

  if (node_to_delete == tree->sentinel)
  {
    return 1;
  }
  else
  {
    tree->num_items--;
    _mcl_rbtree_delete(tree, node_to_delete);

#ifdef DEBUG
    assert(_mcl_rbtree_validate_pointers(tree, tree->root, tree->sentinel));
    assert(_mcl_rbtree_verify(tree));
#endif

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

uint32_t _mcl_rbtree_depth(MCLRBTree *tree, MCLRBTreeNode *root)
{
  if (root == tree->sentinel)
  {
    return 0;
  }

  uint32_t left_depth = _mcl_rbtree_depth(tree, root->left);
  uint32_t right_depth = _mcl_rbtree_depth(tree, root->right);

  return 1 + ((left_depth > right_depth) ? left_depth : right_depth);
}

uint32_t mcl_rbtree_depth(MCLRBTree *tree)
{
  return _mcl_rbtree_depth(tree, tree->root);
}

uint32_t mcl_rbtree_num_items(MCLRBTree *tree)
{
  return tree->num_items;
}

void _mcl_rbtree_visit_pre_order_helper(MCLRBTree *tree
                                       ,MCLRBTreeNode *node
                                       ,MCLVisitor visitor
                                       ,void *user_data)
{
  if (node == tree->sentinel)
  {
    return;
  }
  else
  {
    _mcl_rbtree_visit_pre_order_helper(tree, node->left, visitor, user_data);
    visitor(node->data, user_data);
    _mcl_rbtree_visit_pre_order_helper(tree, node->right, visitor, user_data);
  }
}

void mcl_rbtree_visit_pre_order(MCLRBTree *tree
                               ,MCLVisitor visitor
                               ,void *user_data)
{
  _mcl_rbtree_visit_pre_order_helper(tree, tree->root, visitor, user_data);
}
