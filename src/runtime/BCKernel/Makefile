#	|			|			|			|
include ../../../Makefile.inc

#--------------------------------------------------------------------------------------------------------------
# Sources
#--------------------------------------------------------------------------------------------------------------

SRC=	hashtable.c 		module.c		iofuncs.c		main.c		\
	primitive.c		pretty.c		heap.c			mutator.c       \
	integer.c		mark.c			jonkers.c		sanity.c	\
	external.c		info.c			make.c			foreign.c	\
	process.c  		thread.c  		ffi.c			stable.c	

HATSRC=	hat.c			hat-c.c

BSRC=	

PSRC=  	Prelude.c		IO.c			FFI.c			System.c	\
	PackedString.c		Concurrent.c		Array.c

#--------------------------------------------------------------------------------------------------------------
# Defines
#--------------------------------------------------------------------------------------------------------------

WSRC=$(BSRC:%.c=Wrap%.c)
BWSRC=$(BSRC) $(WSRC) $(PSRC)

NHC_BASE=../../../inst
DST=$(NHC_BASE)/bin

PROFILE=# -fprofile-arcs -ftest-coverage
OPT=
DBGSYM=-g

CC=gcc
CFLAGS+=$(PROFILE) -c -Wall $(OPT) $(DBGSYM) -I../../../ -I../libffi/include $(HATFLAGS) $(CPPFLAGS)

LD=gcc
LDFLAGS+=$(PROFILE) $(OPT) $(DBGSYM)
LDLIBS=-lm $(LIBS) ../libffi/.libs/libffi.a
AR=ar
ARFLAGS=rcs

YHC_BIN=$(DST)/yhi
HAT_BIN=$(DST)/yhi-hat

#--------------------------------------------------------------------------------------------------------------
# Common rules
#--------------------------------------------------------------------------------------------------------------

all: $(YHC_BIN) $(HAT_BIN)

wc:
	wc -l $(SRC)

clean:
	rm -f $(OBJ) $(DEP) $(YHC_BIN) $(HAT_OBJ) $(HAT_BIN) $(HAT_DEP)

#--------------------------------------------------------------------------------------------------------------
# Standard YHC
#--------------------------------------------------------------------------------------------------------------

DEP=$(SRC:%.c=obj/%.d) 
OBJ=$(SRC:%.c=obj/%.o) $(BWSRC:%.c=obj/%.o) 

$(YHC_BIN): $(OBJ)
	$(LD) $(LDFLAGS) -o $@ $^ $(LDLIBS)

include $(DEP)

obj/%.o: %.c ../../../config.h
	$(CC) $(CFLAGS) -o $@ $<

obj/%.o: builtin/%.c ../../../config.h
	$(CC) $(CFLAGS) -o $@ $<

obj/%.o: hat/%.c ../../../config.h
	$(CC) $(CFLAGS) -o $@ $<

# Dependency 'Magic'

obj/%.d: %.c
	set -e; \
	$(CPP) -MM $(CFLAGS) $< \
	| sed 's/\($*\)\.o[ :]*/$(@D)\/\1.o $(@D)\/$(@F) : /g' > $@; \
	[ -s $@ ] || rm -f $@l

obj/%.d: hat/%.c
	set -e; \
	$(CPP) -MM $(CFLAGS) $< \
	| sed 's/\($*\)\.o[ :]*/$(@D)\/\1.o $(@D)\/$(@F) : /g' > $@; \
	[ -s $@ ] || rm -f $@l

#--------------------------------------------------------------------------------------------------------------
# YHC With Hat Support
#--------------------------------------------------------------------------------------------------------------

HAT_DEP=$(SRC:%.c=hatobj/%.d) $(HATSRC:%.c=hatobj/%.d)
HAT_OBJ=$(SRC:%.c=hatobj/%.o) $(BWSRC:%.c=hatobj/%.o) $(HATSRC:%.c=hatobj/%.o)

HAT_CFLAGS=$(CFLAGS) -DHAT

$(HAT_BIN): $(HAT_OBJ)
	$(LD) $(LDFLAGS) -o $@ $^ $(LDLIBS)

include $(HAT_DEP)

hatobj/%.o: %.c ../../../config.h
	$(CC) $(HAT_CFLAGS) -o $@ $<

hatobj/%.o: builtin/%.c ../../../config.h
	$(CC) $(HAT_CFLAGS) -o $@ $<

hatobj/%.o: hat/%.c ../../../config.h
	$(CC) $(HAT_CFLAGS) -o $@ $<


# Dependency 'Magic'

hatobj/%.d: %.c
	set -e; \
	$(CPP) -MM $(HAT_CFLAGS) $< \
	| sed 's/\($*\)\.o[ :]*/$(@D)\/\1.o $(@D)\/$(@F) : /g' > $@; \
	[ -s $@ ] || rm -f $@l

hatobj/%.d: hat/%.c
	set -e; \
	$(CPP) -MM $(HAT_CFLAGS) $< \
	| sed 's/\($*\)\.o[ :]*/$(@D)\/\1.o $(@D)\/$(@F) : /g' > $@; \
	[ -s $@ ] || rm -f $@l


	
