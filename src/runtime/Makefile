
.PHONY: all builtin kernel libffi clean

all: kernel builtin

kernel: libffi
	$(MAKE) -C BCKernel 

libffi:
	$(MAKE) -C libffi -f ConfigAndMake.makefile

clean:
	$(MAKE) -C BCKernel clean


