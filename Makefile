CC         = gcc
GHC        = ghc
GHC_FLAGS  = -g -Wall
HSC2HS     = hsc2hs
CFLAGS  	 = -g -Wall #-DDEBUG

INCLUDES   = -I.

all: MCLListTest MCLRBTreeTest rbtree_main mcl_utility.h libmcl.a list_main

libmcl.a: mcl_list.o mcl_rbtree.o mcl_utility.o
	ar rvs libmcl.a $^

MCLListTest: MCLListTest.hs MCLList.hs mcl_list.o mcl_utility.o
	$(GHC) $(GHC_FLAGS) $^

MCLRBTreeTest: MCLRBTreeTest.hs MCLRBTree.hs mcl_rbtree.o mcl_utility.o MCLList.hs mcl_list.o
	$(GHC) $(GHC_FLAGS) $^

rbtree_main: rbtree_main.o libmcl.a
	$(CC) $(CFLAGS) $(INCLUDES) $^ -o $@

list_main: list_main.o libmcl.a
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
	rm -f *.a
