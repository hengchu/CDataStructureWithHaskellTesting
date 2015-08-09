#include <stdio.h>
#include <assert.h>
#include <mcl_list.h>

int main(int argc, char *argv[])
{
  int data[] = { 265,30,121,181,388,425,435,256,461,2,3,118 };
  
  MCLList *list = mcl_list_create();

  int num_items = sizeof(data) / sizeof(*data);
  
  for (int i = 0; i < num_items; i++)
  {
    mcl_list_append_front(list, data[i]);
  }

  mcl_list_sort(list);
  mcl_list_reverse(list);

  mcl_list_destroy(list);

  return 0;
}
