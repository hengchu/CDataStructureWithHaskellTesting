CC         = gcc
GHC        = ghc
HSC2HS     = hsc2hs
CPP_FLAGS  = -g -Wall

INCLUDES   = -I.

MCLListTest: MCLListTest.hs MCLList.hs mcl_list.o
	$(GHC) $^

%.hs: %.hsc
	$(HSC2HS) $(INCLUDES) $< -o $@

.c.o:
	$(CC) $(INCLUDES) -c $< -o $@

clean:
	rm -f *.o *.hi
	rm -f MCLList.hs
	rm -f MCLListTest
