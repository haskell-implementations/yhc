SRCS   = GtkGui.hs CmdLine.hs Interpreter.hs 

DST    = ../../inst/bin

.PHONY: all gui cmdline

all: gui cmdline

gui: 
	ghc -cpp -fglasgow-exts --make GuiGtk -o $(DST)/gyhe

cmdline: 
	ghc -cpp -fglasgow-exts --make CmdLine -o $(DST)/yhe

clean:
	rm -f $(DST)/yhe $(DST)/gyhe $(SRCS:%.hs=%.hi) $(SRCS:%.hs=%.o)
