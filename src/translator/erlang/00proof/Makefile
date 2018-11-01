YHC = yhc
GHC = ghc
ERLC = erlc
YHCBASE := $(shell dirname $(shell dirname $(shell which yhc)))

YHCSRC  = ../../../../src/compiler98
YHCCORE = ../../../../src/libraries/core
YHCGEN  = ../../../../src/libraries/general
UNIPLATE = ../../../../depends/uniplate


include $(YHCBASE)/lib/mk/HsWebPage.mk

all : check y2e Test1.yca hserl.beam hs_test1.beam

check::
	$(YHC) -v
	$(ERLC) -v

y2e :: y2e.hs
	$(GHC) --make y2e.hs -cpp -i$(YHCCORE) -i$(UNIPLATE) -o y2e
	strip y2e

hserl.beam : hserl.erl
	erlc hserl.erl

hs_test1.beam :: y2e Test1.yca
	./y2e Test1.yca "Test1;main" "Test1;fac" "Test1;fuse" >hs_test1.core
	erlc hs_test1.core

clean:
	rm -f *.hi *.o *.yca y2e

