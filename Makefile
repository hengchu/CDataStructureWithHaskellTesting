CC         = gcc
GHC        = ghc
GHC_FLAGS  = -g
HSC2HS     = hsc2hs
CFLAGS  	 = -g -Wall

INCLUDES   = -I.

rbtree_main: rbtree_main.o mcl_rbtree.o mcl_utility.o
	$(CC) $(CFLAGS) $(INCLUDES) $^ -o $@

MCLListTest: MCLListTest.hs MCLList.hs mcl_list.o mcl_utility.o
	$(GHC) $(GHC_FLAGS) $^

%.hs: %.hsc
	$(HSC2HS) $(INCLUDES) $< -o $@

.c.o:
	$(CC) $(CFLAGS) $(INCLUDES) -c $< -o $@

clean:
	rm -f *.o *.hi
	rm -f MCLList.hs
	rm -f MCLListTest
