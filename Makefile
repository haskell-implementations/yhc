#include Makefile.inc

#.PHONY:	all runtime compiler cpphs yhc-base haskell98 tests

all:
	echo "Yhc no longer builds with make. Please type 'scons' (without the quotes)."
#runtime compiler yhc-base haskell98

# CPPHS=depends/cpphs/.cpphs
# CPPHS_URL=http://www.cs.york.ac.uk/fp/darcs/cpphs
# 
# $(CPPHS):
# 	mkdir -p depends
# 	darcs get --partial --repo-name=depends/cpphs $(CPPHS_URL)
# 	touch $(CPPHS)
# 	$(MAKE) -C depends/cpphs HC="$(HC) --make" STRIP=strip
# 
# Makefile.inc:
# 	$(error Please run 'sh configure' first)
# 
# runtime: $(CPPHS) 
# 	$(MAKE) -C src/runtime
# 
# compiler: $(CPPHS)
# 	$(MAKE) -C src/compiler98
# 
# yhc-base: $(CPPHS)
# 	$(MAKE) -C src/packages/yhc-base-1.0
# 
# haskell98: $(CPPHS)
# 	$(MAKE) -C src/packages/haskell98-1.0
# 
# pull:
# 	darcs pull
# 	darcs pull --repodir=depends/cpphs
# 
# tests: runtime compiler yhc-base haskell98
# 	$(MAKE) -C src/tester tests LOGDIR=$(LOGDIR)
# 
# clean:
# 	$(MAKE) -C src/runtime clean
# 	$(MAKE) -C src/compiler98 clean
# 	$(MAKE) -C src/packages/yhc-base-1.0 clean
# 	$(MAKE) -C src/packages/haskell98-1.0 clean
