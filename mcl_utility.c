#include <mcl_utility.h>

int mcl_default_comparator(MCLItemType item_left
                           ,MCLItemType item_right
                           ,void *ud)
{
  return (item_left < item_right) ? -1 : 1;
}

