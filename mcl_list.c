#include <mcl_list.h>

#ifndef KERNEL
#include <stdlib.h>
#include <stdio.h>
#endif

#ifdef KERNEL
#define malloc k_malloc
#define free   k_free
#endif

typedef struct MCLListNode {
  MCLItemType         data;
  struct MCLListNode *next;
} MCLListNode;

struct MCLList {
  uint32_t     num_items;
  MCLListNode *head;
};

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
