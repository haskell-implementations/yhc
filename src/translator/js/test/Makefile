YHC = yhc

YCR2JS = ../inst/bin/ycr2js

PGBUILD = ../inst/bin/pgbuild

XMLTMPL = ../lib/xhtml/emptyx.html

RUNTIME = ../lib/javascript/Runtime.js

%.yca: %.hs
	$(YHC) -includes ../lib/haskell -linkcore $<

%.js: %.yca
	$(YCR2JS) $< Test1.main > $@

all:	Test1.html

Test1.html: Test1.js
	$(PGBUILD) -t $(XMLTMPL) -o $@ -T "$<" -e $(RUNTIME) -e $< \
		--onload="exprEval(funIdx['Test1.main'])"

