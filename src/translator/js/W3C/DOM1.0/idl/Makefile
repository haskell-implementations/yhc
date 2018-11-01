IDLCOMPILER = omniidl2
IDLHEADER   = hh
IDLSTUBS    = cc

validate-idl::
	$(IDLCOMPILER) org-w3c-dom.idl
	$(IDLCOMPILER) org-w3c-dom-html.idl
	echo "IDL validation succeeded" >> ./validate-output.txt

clean:
	rm -f *~* ./validate-output.txt
	rm -f *.$(IDLHEADER) *.$(IDLSTUBS)

realclean: clean
	rm -f *.idl */*.idl *.$(IDLHEADER) *.$(IDLSTUBS)




