#include <mcl_list.h>

#ifndef KERNEL
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#endif

#ifdef KERNEL
#define malloc k_malloc
#define free   k_free
#define assert k_assert
#endif

typedef struct MCLListNode {
  MCLItemType         data;
  struct MCLListNode *next;
} MCLListNode;

struct MCLList {
  uint32_t     num_items;
  MCLListNode *head;
};

// Write the nth node starting from head into out.
// Return zero on success.
// Return non-zero value on failure.
// head MUST be either NULL (treated as empty list)
// or a valid MCLListNode pointer.
uint8_t _mcl_list_nth_node(MCLListNode *head
                          ,uint32_t n
                          ,MCLListNode **out)
{
  if (NULL == head)
  {
    return 1;
  }

  MCLListNode *curr = head;
  uint32_t     idx  = 0;

  while (curr && idx < n)
  {
    curr = curr->next;
    idx++;
  }

  if (curr)
  {
    *out = curr;
    return 0;
  }

  return 1;
}

MCLList *mcl_list_create()
{
  MCLList *list   = malloc(sizeof(MCLList));

  list->num_items = 0;
  list->head      = NULL;

  return list;
}

void mcl_list_append_front(MCLList *list, MCLItemType item)
{
  MCLListNode *node = malloc(sizeof(MCLListNode));

  node->data = item;
  node->next = list->head;

  list->head = node;
  list->num_items++;
}

int mcl_list_remove(MCLList *list, MCLItemType item_to_remove)
{
  MCLListNode *curr = list->head;
  MCLListNode *prev = NULL;

  // Delete at head
  if (curr && curr->data == item_to_remove)
  {
    list->head = curr->next;
    list->num_items--;
    free(curr);
    return 0;
  }

  while (curr && curr->data != item_to_remove)
  {
    prev = curr;
    curr = curr->next;
  }

  // Delete if there exists such an item
  if (curr) {
    prev->next = curr->next;
    list->num_items--;
    free(curr);
    return 0;
  }

  // No such item.
  return 1;
}

void mcl_list_destroy(MCLList *list)
{
  MCLListNode *curr = list->head;
  MCLListNode *next = NULL;

  while (curr)
  {
    next = curr->next;
    free(curr);
    curr = next;
  }

  free(list);
}

uint32_t mcl_list_num_items(MCLList *list)
{
  return list->num_items;
}

uint8_t mcl_list_empty(MCLList *list)
{
  return list->num_items == 0;
}

uint8_t mcl_list_in_list(MCLList *list, MCLItemType item)
{
  if (mcl_list_empty(list))
  {
    return 0;
  }

  MCLListNode *curr = list->head;

  while (curr)
  {
    if (curr->data == item)
    {
      return 1;
    }

    curr = curr->next;
  }

  return 0;
}

uint8_t mcl_list_in_list_if(MCLList *list, MCLPredicate pred, void* user_data)
{
  if (mcl_list_empty(list))
  {
    return 0;
  }

  MCLListNode *curr = list->head;

  while (curr)
  {
    if (pred(curr->data, user_data))
    {
      return 1;
    }

    curr = curr->next;
  }

  return 0;
}

uint8_t mcl_list_head(MCLList *list, MCLItemType *out)
{
  if (mcl_list_empty(list))
  {
    return 1;
  }
  else
  {
    *out = list->head->data;
    return 0;
  }
}

uint8_t mcl_list_nth_item(MCLList *list, uint32_t n, MCLItemType *out)
{
  if (mcl_list_empty(list))
  {
    return 1;
  }

  MCLListNode *out_node = NULL;

  if (_mcl_list_nth_node(list->head, n, &out_node))
  {
    return 1;
  }
  else
  {
    *out = out_node->data;
    return 0;
  }
}

void mcl_list_reverse(MCLList *list)
{
  if (mcl_list_empty(list) ||
      mcl_list_num_items(list) == 1)
  {
    return;
  }

  MCLListNode *curr = list->head;
  MCLListNode *prev = NULL;
  MCLListNode *next = NULL;

  while (curr)
  {
    next       = curr->next;

    curr->next = prev;

    prev       = curr;
    curr       = next;
  }

  list->head = prev;
}

// Merge two sorted lists, return the new head.
// Nodes are sorted in ASC order.
MCLListNode *_mcl_list_merge_node_with(MCLListNode *head_left
                                      ,MCLListNode *head_right
                                      ,MCLComparator cmp
                                      ,void *user_data)
{

  assert(cmp);

  if (!head_left)
  {
    return head_right;
  }

  if (!head_right)
  {
    return head_left;
  }

  if (cmp(head_left->data, head_right->data, user_data) < 0)
  {
    head_left->next = _mcl_list_merge_node_with(head_right
                                               ,head_left->next
                                               ,cmp
                                               ,user_data);
    return head_left;
  }
  else
  {
    head_right->next = _mcl_list_merge_node_with(head_left
                                                ,head_right->next
                                                ,cmp
                                                ,user_data);
    return head_right;
  }
}

// A helper function to mcl_list_sort.
// It operates on the list node directly,
// return the new head of the list.
// list_head MUST be either NULL (in which case, length MUST be zero)
// or a valid MCLListNode pointer.
MCLListNode *_mcl_list_sort_node_with(MCLListNode *list_head
                                     ,uint32_t length
                                     ,MCLComparator cmp
                                     ,void *user_data)
{
  if (length == 1)
  {
    return list_head;
  }

  if (length == 0)
  {
    assert(list_head == NULL);
    return list_head;
  }

  MCLListNode *list1 = list_head;
  MCLListNode *list1_tail = NULL;

  uint32_t middle = (length - 1) / 2;
  uint8_t rc = _mcl_list_nth_node(list_head, middle, &list1_tail);

  assert(0 == rc);
  assert(NULL != list1_tail);

  MCLListNode *list2 = list1_tail->next;

  // Seperate the two lists,
  // so that they can be sorted seperately.
  list1_tail->next = NULL;

  list1 = _mcl_list_sort_node_with(list1, middle + 1, cmp, user_data);
  list2 = _mcl_list_sort_node_with(list2, length - middle - 1, cmp, user_data);

  return _mcl_list_merge_node_with(list1, list2, cmp, user_data);
}

void mcl_list_sort(MCLList *list)
{
  mcl_list_sort_with(list, mcl_default_comparator, NULL);
}

void mcl_list_sort_with(MCLList *list, MCLComparator cmp, void *user_data)
{
  uint32_t length = mcl_list_num_items(list);

  if (mcl_list_empty(list) ||
      length == 1)
  {
    return;
  }

  list->head = _mcl_list_sort_node_with(list->head
                                       ,length
                                       ,cmp
                                       ,user_data);
}
