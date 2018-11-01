BASE=../../../inst
YHC_BASE = $(BASE)/lib/yhc/packages/haskell98/1.0/

DST=$(YHC_BASE)
YHC=YHC_BASE_PATH=$(BASE) ; export YHC_BASE_PATH ; $(BASE)/bin/yhc -d $(DST) -i $(DST)

.PHONY: all clean

#------------------------------------------------------------------------------------

ROOT_HS = Ix.hs       Array.hs    Char.hs  Maybe.hs System.hs \
          Complex.hs  IO.hs       List.hs  Monad.hs  Ratio.hs 
ROOT_HI = $(ROOT_HS:%.hs=$(DST)/%.hi)

#------------------------------------------------------------------------------------

ALL_HI = $(ROOT_HI) 

#------------------------------------------------------------------------------------

all: $(DST)/.exists $(ALL_HI)

clean:
	rm -f $(ALL_HI)

$(DST)/.exists:
	mkdir -p $(DST)
	touch $@

$(DST)/%.hi:			%.hs
	$(YHC)			$<

#------------------------------------------------------------------------------------
# *
#------------------------------------------------------------------------------------

#Array.hi:	Array.hs	\
#	Ix.hi

#Char.hi:	Char.hs		
#
#Complex.hi:	Complex.hs
#
#
#CString.hi:	CString.hs	\
#	NHC/PackedString.hi	
#
#_Driver.hi:	_Driver.hs	\
#	System.hi
#
##IO.hi:		IO.hs		\
#	NHC/ErrNo.hi 
#
##Ix.hi:		Ix.hs
##
#List.hi:	List.hs		\
#	#Maybe.hi
#
##Maybe.hi:	Maybe.hs
#
#Monad.hi:	Monad.hs
#
#N#umeric.hi:	Numeric.hs	\
#	Array.hi Char.hi Ratio.hi
#
#NonStdTrace.hi:	NonStdTrace.hs	\
#	IO.hi
#
#NonStdUnsafeCoerce.hi: NonStdUnsafeCoerce.hs 
#	$(NHC) $<
#        # restore the old (and hacked) hi file
#	cp saved/NonStdUnsafeCoerce.hi . 
#
#PreludeAux.hi:  PreludeAux.hs 	\
#	#Ratio.hi
#
#Prelude.hi:	Prelude.hs	\
#	System.hi IO.hi  Numeric.hi Ratio.hi Char.hi NonStdTrace.hi PreludeAux.hi
#	$(NHC) -redefine -prelude $<
#
#Ratio.hi:	Ratio.hs
#
#System.hi:	System.hs	

#------------------------------------------------------------------------------------

