#============ Begin Simple Makefile for Standalone ycr2js ============

YHC = yhc
YHCBASE = $$(dirname $$(dirname `which $(YHC)`))
YCR2JS =  $(YHCBASE)/bin/ycr2js
PGBUILD = $(YHCBASE)/bin/pgbuild
XMLTMPL = $(YHCBASE)/lib/xhtml/emptyx.html
RUNTIME = $(YHCBASE)/lib/javascript/Runtime.js
JUCD    = $(YHCBASE)/lib/javascript/JUCD.js
HSJSLIB = $(YHCBASE)/lib/haskell

# Insert names of Web pages to build here. General naming rule:
# if the page is built out of a Haskell file Foo.hs which contains
# the main function, the Web page file name will be Foo.html

# If Foo.hs imports modules from any location other than current directory,
# add more -includes options to this rule using the HSINCLUDES make variable.

%.yca: %.hs
	$(YHC) -I $(HSJSLIB) $(HSINCLUDES) --linkcore --no-bytecode $<

%.js: %.yca
	$(YCR2JS) $< $*\;main > $@

# These two rules generate visual representation of Core.
# Include files with names ending with .yc[ra]txt in the list
# of `all' dependencies to build these files: .yca and .js files
# will be deleted by make in the very end.

%.ycatxt: %.yca
	$(YHC) --viewcore $< > $@

%.ycrtxt: %.ycr
	$(YHC) --viewcore $< > $@

# Don't forget that your "root" module must contain the
# `main' function. Its type signature does not matter.
# Use the indirect reference via funIdx for the page entry point
# because functions are now indexed to have shorter names.

%.html: %.js
	$(PGBUILD) -t $(XMLTMPL) -o $@ -T "$*" -e $(RUNTIME) -e $(JUCD) -e $< \
		--onload="exprEval(funIdx['$*\;main'])"

#============= End Simple Makefile for Standalone ycr2js =============

