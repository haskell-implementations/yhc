HTML = NewEntry.html MainGUI.html
ISP2PB = ./isp2pb
RUNYHC = ./runyhc
SPYHC  = ./spyhc
DBINIT = ./dbinit
YWSD = ./ywsd
LOGCUT = ./logcut
BIN = $(ISP2PB) $(RUNYHC) $(SPYHC) $(DBINIT)
GHC = ghc
YHCREPO = ../translator/js
YWSLIB = lib/yws

all:	$(HTML) $(BIN)

isp2pb: isp2pb.hs
	$(GHC) -i$(YHCREPO) --make \
	-package base -package network \
	-package HTTP -package filepath \
	-package base64-string -package utf8-string $< -o $@
	strip $@
		
runyhc: runyhc.hs
	$(GHC) -i$(YHCREPO) --make \
	-package base -package network \
	-package HTTP -package filepath \
	-package base64-string -package utf8-string $< -o $@
	strip $@

spyhc:  spyhc.hs
	$(GHC) --make $< -o $@
	strip $@
			
dbinit:	dbinit.hs
	$(GHC) -i$(YHCREPO) --make \
	-package base -package network \
	-package HTTP -package filepath $< -o $@
	strip $@

template.html: template.html.in
	cp template.html.in template.html

YHCBASE := $(shell dirname $(shell dirname $(shell which yhc)))

include $(YHCBASE)/lib/mk/HsWebPage.mk

HSINCLUDES = --cpp

XMLTMPL = template.html

$(HTML) : $(shell ls *.hs) $(XMLTMPL)

clean:
	-rm -f *.o *.hi *.ycr *.yca *.hbc *.html $(BIN)

CDBUP = $(YHCBASE)/bin/cdbup

BASE = http://localhost:5984

DOC = $(BASE)/static/yhcws

install:  all install-bin install-mod upload

install-bin: $(BIN)
	cp $(BIN) $(YWSD) $(LOGCUT) $(YHCBASE)/bin

install-mod:
	mkdir -p $(YHCBASE)/$(YWSLIB)
	cp YWSWidgets.hi YWSWidgets.ycr $(YHCBASE)/$(YWSLIB)

init-db:
	$(DBINIT) $(BASE)

upload:
	$(CDBUP) $(DOC) template.html -c 'text/html;charset=utf-8' ||\
	$(CDBUP) $(DOC) template.html -n -c 'text/html;charset=utf-8'
	$(CDBUP) $(DOC) NewEntry.html -c 'text/html;charset=utf-8'
	$(CDBUP) $(DOC) MainGUI.html -c 'text/html;charset=utf-8'
	$(CDBUP) $(DOC) hssrc.png -b -c 'image/png'
	$(CDBUP) $(DOC) error.png -b -c 'image/png'
	$(CDBUP) $(DOC) log.png -b -c 'image/png'
	$(CDBUP) $(DOC) webpage.png -b -c 'image/png'
	$(CDBUP) $(DOC) edit.png -b -c 'image/png'

