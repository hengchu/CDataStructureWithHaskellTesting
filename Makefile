CC         = gcc
GHC        = ghc
GHC_FLAGS  = -g -Wall
HSC2HS     = hsc2hs
CFLAGS  	 = -g -Wall #-DDEBUG

INCLUDES   = -I.

all: MCLListTest MCLRBTreeTest rbtree_main mcl_utility.h

MCLListTest: MCLListTest.hs MCLList.hs mcl_list.o mcl_utility.o
	$(GHC) $(GHC_FLAGS) $^

MCLRBTreeTest: MCLRBTreeTest.hs MCLRBTree.hs mcl_rbtree.o mcl_utility.o MCLList.hs mcl_list.o
	$(GHC) $(GHC_FLAGS) $^

rbtree_main: rbtree_main.o mcl_rbtree.o mcl_utility.o
	$(CC) $(CFLAGS) $(INCLUDES) $^ -o $@

%.hs: %.hsc
	$(HSC2HS) $(INCLUDES) $< -o $@

.c.o:
	$(CC) $(CFLAGS) $(INCLUDES) -c $< -o $@

clean:
	rm -f *.o *.hi
	rm -f MCLList.hs
	rm -f MCLListTest
	rm -f MCLRBTree.hs
	rm -f MCLRBTreeTest
