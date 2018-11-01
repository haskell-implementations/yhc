#	A temporary Makefile to build the Yhc core to Javascript
#	converter. Eventually this directory gets sconsified, 
#	and the converter will be built from the toplevel scons.

#	Programs

GHC = ghc

YHC = yhc

IHC = ihc

HAPPY = happy -g

# Haddock should be pre 2.0

HADDOCK = haddock 

CPPHS_BIN = cpphs

#	Directories

INSTDIR = $$(dirname $$(dirname `which $(YHC)`))


YHCSRC  = ../../compiler98
YHCCORE = ../../libraries/core
YHCGEN  = ../../libraries/general
FILEPATH = ../../../depends/filepath
CPPHS = ../../../depends/cpphs
PLAY = ../../../depends/play
UNIPLATE = ../../../depends/uniplate

PWD := $(shell pwd)

HJSBASE := $(PWD)/lib/haskell

DOCBASE := $(PWD)/docs

YHCPKGS := $(PWD)/../../packages/yhc-base-1.0

YHCWS := $(PWD)/../../yhcws

DOMLEVEL := $(HJSBASE)/DOM/Level

IDLSRC = W3C

HDIRECT = ./hdirect-0.21

#	Targets

PGBUILD := $(PWD)/inst/bin/pgbuild
YCR2JS  := $(PWD)/inst/bin/ycr2js
DOMCONV := $(PWD)/inst/bin/domconv
SPLITTER := $(PWD)/inst/bin/splitter

TOOLS = $(PGBUILD) $(YCR2JS) $(DOMCONV) $(SPLITTER)

all: yhcchk $(HDIRECT)/OmgParser.hs $(TOOLS) idlhs overlay

.PRECIOUS: %.hs

idlhs:: $(IDLSRC)/addtags.idl \
	$(DOMLEVEL)1/Dom.hs \
	$(DOMLEVEL)1/Html.hs \
	$(DOMLEVEL)2/Dom.hs \
	$(DOMLEVEL)2/Html2.hs \
	$(DOMLEVEL)2/Views.hs \
	$(DOMLEVEL)2/Events.hs \
	$(DOMLEVEL)2/Stylesheets.hs \
	$(DOMLEVEL)2/Css.hs \
	$(DOMLEVEL)2/Ranges.hs \
	$(DOMLEVEL)2/Traversal.hs \
	$(DOMLEVEL)2/XMLHTTP.hs

# Add definitions for tags that only have basic HTMLElement interface

#    * special: SUB, SUP, SPAN, BDO
#    * font: TT, I, B, U, S, STRIKE, BIG, SMALL
#    * phrase: EM, STRONG, DFN, CODE, SAMP, KBD, VAR, CITE, ACRONYM, ABBR
#    * list: DD, DT
#    * NOFRAMES, NOSCRIPT
#    * ADDRESS, CENTER


$(IDLSRC)/addtags.idl :
	echo > $@
	for tag in SUB SUP SPAN BDO I B U S \
		STRIKE BIG SMALL EM STRONG DFN CODE \
		SAMP KBD VAR CITE ACRONYM ABBR \
		DD DT NOFRAMES NOSCRIPT ADDRESS CENTER ; do \
		        ltag=`echo $${tag:1} | tr [:upper:] [:lower:]` ; \
			echo "  interface HTML$${tag:0:1}$${ltag}Element : HTMLElement {" >> $@ ; \
			echo "  };" >> $@ ; \
			echo >> $@ ; \
		done

$(DOMLEVEL)1/Dom.hs_unsplit :: $(IDLSRC)/DOM1.1/idl/dom.idl
	mkdir -p `dirname $@`
	(echo '#pragma namespace "DOM.Level1"'; cat $<) \
		| $(DOMCONV) -I`dirname $<` > $@

$(DOMLEVEL)1/Html.hs_unsplit :: $(IDLSRC)/DOM1.1/idl/html.idl
	mkdir -p `dirname $@`
	(echo '#pragma namespace "DOM.Level1"'; cat $<) \
		| $(DOMCONV) -I`dirname $<` > $@

$(DOMLEVEL)2/Dom.hs_unsplit :: $(IDLSRC)/DOM2/idl/dom.idl
	mkdir -p `dirname $@`
	(echo '#pragma namespace "DOM.Level2"'; cat $<) \
		| $(DOMCONV) -I`dirname $<` > $@

$(DOMLEVEL)2/Html2.hs_unsplit :: $(IDLSRC)/DOM2/idl/html2.idl
	mkdir -p `dirname $@`
	(echo '#pragma namespace "DOM.Level2"'; cat $<) \
		| $(DOMCONV) -I`dirname $<` > $@

$(DOMLEVEL)2/Views.hs_unsplit :: $(IDLSRC)/DOM2/idl/views.idl
	mkdir -p `dirname $@`
	(echo '#pragma namespace "DOM.Level2"'; cat $<) \
		| $(DOMCONV) -I`dirname $<` > $@

$(DOMLEVEL)2/Events.hs_unsplit :: $(IDLSRC)/DOM2/idl/events.idl
	mkdir -p `dirname $@`
	(echo '#pragma namespace "DOM.Level2"'; cat $<) \
		| $(DOMCONV) -I`dirname $<` > $@

$(DOMLEVEL)2/Stylesheets.hs_unsplit :: $(IDLSRC)/DOM2/idl/stylesheets.idl
	mkdir -p `dirname $@`
	(echo '#pragma namespace "DOM.Level2"'; cat $<) \
		| $(DOMCONV) -I`dirname $<` > $@

$(DOMLEVEL)2/Css.hs_unsplit :: $(IDLSRC)/DOM2/idl/css.idl
	mkdir -p `dirname $@`
	(echo '#pragma namespace "DOM.Level2"'; cat $<) \
		| $(DOMCONV) -I`dirname $<` > $@

$(DOMLEVEL)2/Ranges.hs_unsplit :: $(IDLSRC)/DOM2/idl/ranges.idl
	mkdir -p `dirname $@`
	(echo '#pragma namespace "DOM.Level2"'; cat $<) \
		| $(DOMCONV) -I`dirname $<` > $@

$(DOMLEVEL)2/Traversal.hs_unsplit :: $(IDLSRC)/DOM2/idl/traversal.idl
	mkdir -p `dirname $@`
	(echo '#pragma namespace "DOM.Level2"'; cat $<) \
		| $(DOMCONV) -I`dirname $<` > $@


$(DOMLEVEL)2/Xmlhttp.hs_unsplit :: $(IDLSRC)/DOM2/idl/xmlhttp.idl
	mkdir -p `dirname $@`
	(echo '#pragma namespace "DOM.Level2"'; cat $<) \
		| $(DOMCONV) -I`dirname $<` > $@


$(DOMLEVEL)1/Dom.hs :: $(DOMLEVEL)1/Dom.hs_unsplit
	cd $(HJSBASE) ; $(SPLITTER) $<

$(DOMLEVEL)1/Html.hs :: $(DOMLEVEL)1/Html.hs_unsplit
	cd $(HJSBASE) ; $(SPLITTER) $<


$(DOMLEVEL)2/Dom.hs :: $(DOMLEVEL)2/Dom.hs_unsplit
	cd $(HJSBASE) ; $(SPLITTER) $<

$(DOMLEVEL)2/Html2.hs :: $(DOMLEVEL)2/Html2.hs_unsplit
	cd $(HJSBASE) ; $(SPLITTER) $<

$(DOMLEVEL)2/Views.hs :: $(DOMLEVEL)2/Views.hs_unsplit
	cd $(HJSBASE) ; $(SPLITTER) $<

$(DOMLEVEL)2/Events.hs :: $(DOMLEVEL)2/Events.hs_unsplit
	cd $(HJSBASE) ; $(SPLITTER) $<

$(DOMLEVEL)2/Stylesheets.hs :: $(DOMLEVEL)2/Stylesheets.hs_unsplit
	cd $(HJSBASE) ; $(SPLITTER) $<

$(DOMLEVEL)2/Css.hs :: $(DOMLEVEL)2/Css.hs_unsplit
	cd $(HJSBASE) ; $(SPLITTER) $<

$(DOMLEVEL)2/Ranges.hs :: $(DOMLEVEL)2/Ranges.hs_unsplit
	cd $(HJSBASE) ; $(SPLITTER) $<

$(DOMLEVEL)2/Traversal.hs :: $(DOMLEVEL)2/Traversal.hs_unsplit
	cd $(HJSBASE) ; $(SPLITTER) $<

$(DOMLEVEL)2/XMLHTTP.hs :: $(DOMLEVEL)2/Xmlhttp.hs_unsplit
	cd $(HJSBASE) ; $(SPLITTER) $<


%.hs: %.y
	$(HAPPY) $(HAPPY_OPTS) $< -o $@

%.ycr: %.hs
	$(YHC) --cpp --core --no-bytecode $<

$(SPLITTER): splitter.hs
	$(GHC) --make -i$(FILEPATH) $< -o $@

$(PGBUILD): pgbuild.hs
	$(GHC) --make -package base -package network -package hxt -package filepath $< -o $@
	strip $@

$(YCR2JS)::
	$(GHC) --make 	-i$(YHCSRC) -i$(YHCCORE) -i$(YHCGEN) \
			-i$(FILEPATH) -i$(PLAY) -i$(UNIPLATE) ycr2js.hs -o $@
	strip $@

$(DOMCONV):: domconv.hs
	$(GHC) --make -fglasgow-exts -i$(FILEPATH) -i$(HDIRECT) -i$(YHCSRC) -i$(CPPHS) -i$(PLAY) \
		-package haskell-src domconv.hs -o $@
	find . -name '*.idl' | xargs touch
yhcchk::
	$(YHC) -v

overlay:
	(cd $(HJSBASE); \
	  for f in `find . -name '*.hs' | sort` ; do make -s -f $(PWD)/Makefile `dirname $$f`/`basename $$f .hs`.ycr ; done) 
#	for f in `find . -name '*.hs' | sort` ; do $(YHC) --core --no-bytecode $$f ; done) 

clean:
	rm -f $(TOOLS)
	find . -name '*.hbc' | xargs -r rm
	find . -name '*.ycr' | xargs -r rm
	find . -name '*.yca' | xargs -r rm
	find . -name '*.hi' | xargs -r rm
	find . -name '*.o'  | xargs -r rm
	rm -rf docs
	rm -rf $(HJSBASE)/DOM
#	Commented out as not everybody is able to generate it.
#	The file will be added to the darcs repo.
#	rm -f $(IDLSRC)/addtags.idl

install: all
	(cd inst; cp -R ../lib/* ./lib ; find . -name '*.hs' | xargs rm ; cp -R * $(INSTDIR))

install-docs: docs
	cp docs/html/* $(INSTDIR)/share/doc/yhcws

test::
	(cd test; make -B)


#	Docs are not make automatically: run "make docs"

docs::
	-mkdir -p docs/html
	$(HADDOCK) --version
	$(CPPHS_BIN) --version
	(cd $(HJSBASE); \
	  for f in `find . -name '*.hs'` ; do echo Preprocessing $$f...; mkdir -p $(DOCBASE)/`dirname $$f`; $(CPPHS_BIN) $$f -D__HADDOCK__ --noline > $(DOCBASE)/$$f ; done)
	(cd $(YHCPKGS); \
	  for f in `find . -name '*.hs'` ; do echo Preprocessing $$f...; mkdir -p $(DOCBASE)/`dirname $$f`; $(CPPHS_BIN) $$f -D__HADDOCK__ --noline > $(DOCBASE)/$$f ; done)
	(cd $(YHCWS); \
	  for f in YWSWidgets.hs ; do echo Preprocessing $$f...; mkdir -p $(DOCBASE)/`dirname $$f`; $(CPPHS_BIN) $$f -D__HADDOCK__ --noline > $(DOCBASE)/$$f ; done)
	(cd $(DOCBASE) ; find . -name '*.hs' | xargs haddock -o ./html -h)	

#	CouchDB tools. Not built automatically

CDBUP := $(PWD)/inst/bin/cdbup

cdbtools: $(CDBUP)

$(CDBUP): cdbup.hs
	$(GHC) --make -package base -package network\
	       	      -package HTTP -package filepath\
		      -package base64-string -package utf8-string $< -o $@
	strip $@


