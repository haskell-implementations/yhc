BASE=../../../inst
YHC_BASE = $(BASE)/lib/yhc/packages/yhc-base/1.0/

DST=$(YHC_BASE)
YHC=YHC_BASE_PATH=$(BASE) ; export YHC_BASE_PATH ; $(BASE)/bin/yhc -d $(DST) -i $(DST) -c

.PHONY: all clean

#------------------------------------------------------------------------------------

YHC_HS_ = Internal.hs ErrNo.hs _Driver.hs Primitive.hs
YHC_HS  = $(YHC_HS_:%.hs=YHC/%.hs)
YHC_HI  = $(YHC_HS:%.hs=$(DST)/%.hi)

DATA_HS_ = Array.hs Char.hs Complex.hs Ix.hs List.hs Maybe.hs Ratio.hs _CharNumeric.hs PackedString.hs
DATA_HS = $(DATA_HS_:%.hs=Data/%.hs )
DATA_HI = $(DATA_HS:%.hs=$(DST)/%.hi)

SYSTEM_HS_ = Exit.hs IO.hs Environment.hs Directory.hs
SYSTEM_HS  = $(SYSTEM_HS_:%.hs=System/%.hs)
SYSTEM_HI  = $(SYSTEM_HS:%.hs=$(DST)/%.hi)

FOREIGN_C_HS_ = Types.hs String.hs Error.hs
FOREIGN_C_HS  = $(FOREIGN_C_HS_:%.hs=Foreign/C/%.hs)
FOREIGN_C_HI  = $(FOREIGN_C_HS:%.hs=$(DST)/%.hi)

FOREIGN_MARSHAL_HS_ = Alloc.hs Utils.hs
FOREIGN_MARSHAL_HS  = $(FOREIGN_MARSHAL_HS_:%.hs=Foreign/Marshal/%.hs)
FOREIGN_MARSHAL_HI  = $(FOREIGN_MARSHAL_HS:%.hs=$(DST)/%.hi)

FOREIGN_HS_ = Int.hs Word.hs Ptr.hs ForeignPtr.hs Storable.hs C.hs Util.hs StablePtr.hs
FOREIGN_HS  = $(FOREIGN_HS_:%.hs=Foreign/%.hs)
FOREIGN_HI  = $(FOREIGN_HS:%.hs=$(DST)/%.hi) $(FOREIGN_C_HI) $(FOREIGN_MARSHAL_HI)

CONTROL_CONCURRENT_HS_ = MVar.hs QSem.hs
CONTROL_CONCURRENT_HS  = $(CONTROL_CONCURRENT_HS_:%.hs=Control/Concurrent/%.hs)
CONTROL_CONCURRENT_HI  = $(CONTROL_CONCURRENT_HS:%.hs=$(DST)/%.hi)

CONTROL_HS_  = Monad.hs Concurrent.hs
CONTROL_HS   = $(CONTROL_HS_:%.hs=Control/%.hs)
CONTROL_HI   = $(CONTROL_HS:%.hs=$(DST)/%.hi) $(CONTROL_CONCURRENT_HI)

DEBUG_HS_    = Trace.hs
DEBUG_HS     = $(DEBUG_HS_:%.hs=Debug/%.hs)
DEBUG_HI     = $(DEBUG_HS:%.hs=$(DST)/%.hi)

ROOT_HS = Prelude.hs PreludeAux.hs Foreign.hs Numeric.hs
ROOT_HI = $(ROOT_HS:%.hs=$(DST)/%.hi)

#------------------------------------------------------------------------------------

ALL_HI = $(ROOT_HI) $(DATA_HI) $(YHC_HI) $(SYSTEM_HI) $(FOREIGN_HI) $(CONTROL_HI)

#------------------------------------------------------------------------------------

YHC_BOOTSTRAP=$(YHC_BASE)/.bootstrap
BOOTSTRAP_BASE=$(BASE)/../bootstrap/packages/yhc-base/1.0

all: $(DST)/.exists $(YHC_BOOTSTRAP) $(ALL_HI)
	cp $(BOOTSTRAP_BASE)/Prelude.hi        $(YHC_BASE)

clean:
	rm -f $(ALL_HI) $(YHC_BOOTSTRAP)



$(YHC_BOOTSTRAP):
	mkdir -p $(YHC_BASE)
	cp $(BOOTSTRAP_BASE)/Prelude.hi        $(YHC_BASE)
	cp $(BOOTSTRAP_BASE)/PreludeBuiltin.hi $(YHC_BASE)
	mkdir -p $(YHC_BASE)/YHC
	cp $(BOOTSTRAP_BASE)/YHC/Internal.hi   $(YHC_BASE)/YHC
	mkdir -p $(YHC_BASE)/Data
	cp $(BOOTSTRAP_BASE)/Data/Ratio.hi     $(YHC_BASE)/Data
	cp $(BOOTSTRAP_BASE)/Data/Ratio.hbc    $(YHC_BASE)/Data
	touch $@


$(DST)/.exists:
	mkdir -p $(DST)
	touch $@

#------------------------------------------------------------------------------------
# YHC.*
#------------------------------------------------------------------------------------

$(DST)/YHC/Internal.hbc: 	YHC/Internal.hs
	$(YHC) -redefine $<

$(DST)/YHC/Primitive.hi:	YHC/Primitive.hs

$(DST)/YHC/ErrNo.hi:		YHC/ErrNo.hs

$(DST)/YHC/_Driver.hi:		YHC/_Driver.hs \
					$(DST)/YHC/Internal.hbc \
					$(DST)/System/Exit.hi

#------------------------------------------------------------------------------------
# Data.*
#------------------------------------------------------------------------------------

$(DST)/Data/Array.hi:		Data/Array.hs	\
					$(DST)/Data/Ix.hi
$(DST)/Data/Char.hi:   		Data/Char.hs    \
					$(DST)/Data/_CharNumeric.hi
$(DST)/Data/Complex.hi:	 	Data/Complex.hs
$(DST)/Data/Ix.hi:		Data/Ix.hs
$(DST)/Data/List.hi:		Data/List.hs 	\
					$(DST)/Data/Maybe.hi
$(DST)/Data/Maybe.hi:		Data/Maybe.hs
$(DST)/Data/PackedString.hi:	Data/PackedString.hs \
					$(DST)/YHC/Primitive.hi \
					$(DST)/Foreign/Ptr.hi \
					$(DST)/Foreign/Util.hi
	$(YHC) -unifyhack $<

$(DST)/Data/Ratio.hi:		Data/Ratio.hs
	$(YHC) -unifyhack $<

$(DST)/Data/_CharNumeric.hi:	Data/_CharNumeric.hs



$(DST)/%.hi:		%.hs
	$(YHC) $<

#------------------------------------------------------------------------------------
# System.*
#------------------------------------------------------------------------------------

$(DST)/System/Exit.hi:		System/Exit.hs		\
					$(DST)/YHC/Internal.hbc \
					$(DST)/YHC/Primitive.hi

$(DST)/System/IO.hi:		System/IO.hs		\
					$(DST)/YHC/ErrNo.hi \
					$(DST)/YHC/Internal.hbc \
					$(DST)/YHC/Primitive.hi \
					$(DST)/Foreign.hi \
					$(DST)/Foreign/C.hi \
					$(DST)/Data/Ix.hi
	$(YHC) -unifyhack $<

$(DST)/System/Directory.hi:	System/Directory.hs	\
					$(DST)/YHC/Primitive.hi \
					$(DST)/Foreign.hi \
					$(DST)/Foreign/C.hi \

#------------------------------------------------------------------------------------
# Foreign.C.*
#------------------------------------------------------------------------------------

$(DST)/Foreign/C/Types.hi:	Foreign/C/Types.hs	\
					$(DST)/YHC/Internal.hbc \
					$(DST)/YHC/Primitive.hi \
					$(DST)/Foreign/Int.hi \
					$(DST)/Foreign/Word.hi \
					$(DST)/Foreign/Ptr.hi \
					$(DST)/Foreign/Storable.hi \
					$(DST)/Control/Monad.hi
	$(YHC) -cpp $<

$(DST)/Foreign/C/String.hi:	Foreign/C/String.hs	\
					$(DST)/YHC/Internal.hbc \
					$(DST)/YHC/Primitive.hi \
					$(DST)/Foreign/Util.hi


$(DST)/Foreign/C/Error.hi:	Foreign/C/Error.hs	\
					$(DST)/YHC/Internal.hbc \
					$(DST)/YHC/Primitive.hi \
					$(DST)/YHC/ErrNo.hi
#------------------------------------------------------------------------------------
# Foreign.Marshal
#------------------------------------------------------------------------------------

$(DST)/Foreign/Marshal/Alloc.hi: Foreign/Marshal/Alloc.hs \
					$(DST)/YHC/Primitive.hi \
					$(DST)/Foreign/Ptr.hi \
					$(DST)/Foreign/Storable.hi

$(DST)/Foreign/Marshal/Utils.hi: Foreign/Marshal/Utils.hs \
					$(DST)/YHC/Primitive.hi \
					$(DST)/Foreign/Ptr.hi \
					$(DST)/Foreign/Storable.hi \
					$(DST)/Foreign/Marshal/Alloc.hi

#------------------------------------------------------------------------------------
# Foreign.*
#------------------------------------------------------------------------------------

$(DST)/Foreign/Int.hi:		Foreign/Int.hs		\
					$(DST)/Numeric.hi \
					$(DST)/Data/Ix.hi
	$(YHC) -cpp $<

$(DST)/Foreign/Word.hi:		Foreign/Word.hs		\
					$(DST)/Numeric.hi \
					$(DST)/Data/Ix.hi
	$(YHC) -cpp $<

$(DST)/Foreign/Ptr.hi:		Foreign/Ptr.hs		\
					$(DST)/Numeric.hi \
					$(DST)/YHC/Internal.hbc \
					$(DST)/YHC/Primitive.hi

$(DST)/Foreign/ForeignPtr.hi:	Foreign/ForeignPtr.hs	\
					$(DST)/YHC/Internal.hbc \
					$(DST)/YHC/Primitive.hi \
					$(DST)/Foreign/Ptr.hi

$(DST)/Foreign/Storable.hi:	Foreign/Storable.hs	\
					$(DST)/Foreign/Int.hi \
					$(DST)/Foreign/Word.hi \
					$(DST)/Foreign/Ptr.hi \
					$(DST)/YHC/Internal.hbc \
					$(DST)/YHC/Primitive.hi 

$(DST)/Foreign/C.hi:		Foreign/C.hs		\
					$(DST)/Foreign/C/Types.hi \
					$(DST)/Foreign/C/String.hi \
					$(DST)/Foreign/C/Error.hi

$(DST)/Foreign/Marshal.hi:	Foreign/Marshal.hs	\
					$(DST)/Foreign/Marshal/Alloc.hi \
					$(DST)/Foreign/Marshal/Utils.hi

$(DST)/Foreign/StablePtr.hi:	Foreign/StablePtr.hs 	\
					$(DST)/YHC/Primitive.hi

#------------------------------------------------------------------------------------
# Debug.*
#------------------------------------------------------------------------------------

$(DST)/Debug/Trace.hi:		Debug/Trace.hs		\
					$(DST)/System/IO.hi \
					$(DST)/YHC/Internal.hbc \
					$(DST)/YHC/Primitive.hi 

#------------------------------------------------------------------------------------
# *
#------------------------------------------------------------------------------------

$(DST)/Foreign.hi:		Foreign.hs	\
					$(DST)/Foreign/Ptr.hi \
					$(DST)/Foreign/Int.hi \
					$(DST)/Foreign/Word.hi \
					$(DST)/Foreign/ForeignPtr.hi \
					$(DST)/Foreign/StablePtr.hi \
					$(DST)/Foreign/Storable.hi \
					$(DST)/Foreign/Marshal.hi \
					$(DST)/YHC/Internal.hbc

$(DST)/Numeric.hi:		Numeric.hs  \
					$(DST)/Data/Array.hi \
					$(DST)/Data/Char.hi \
					$(DST)/Data/Ratio.hi \
					$(DST)/Data/_CharNumeric.hi

$(DST)/Prelude.hi:		Prelude.hs	\
					$(DST)/System/Exit.hi \
					$(DST)/System/IO.hi \
					$(DST)/Data/Char.hi \
					$(DST)/Numeric.hi \
					$(DST)/Data/Ratio.hi \
					$(DST)/Data/PackedString.hi \
					$(DST)/YHC/Internal.hbc \
					$(DST)/YHC/Primitive.hi \
					$(DST)/YHC/ErrNo.hi \
					$(DST)/Debug/Trace.hi \
					$(DST)/PreludeAux.hi
	$(YHC) -redefine -prelude $<

#------------------------------------------------------------------------------------

